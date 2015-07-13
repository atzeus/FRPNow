{-# LANGUAGE LambdaCase,RecursiveDo, FlexibleContexts, ExistentialQuantification, Rank2Types,GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.FRPNow.Core
-- Copyright   :  (c) Atze van der Ploeg 2015
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
-- 
-- The core FRPNow interface, based on the paper "Principled Practical FRP: Forget the past, Change the future, FRPNow!", ICFP 2015, by Atze van der Ploeg and Koenem Claessem.
-- 
-- This module contains the core FRPNow interface, which consists of:
--
--  * The pure interface, which has denotational semantics
--  * The IO interface
--  * The entry points, i.e. the functions that are used to start the FRP system.

module Control.FRPNow.Core(
   -- * Pure interface
   -- $time
   Event,Behavior, never, switch, whenJust, futuristic,
  -- * IO interface
   Now, async, asyncOS, callback, sampleNow, planNow,  sync,
  -- * Entry point
   runNowMaster,
   initNow) where
import Control.Concurrent.Chan
import Control.Applicative hiding (empty,Const)
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Control.Monad.Reader  hiding (mapM_)
import Control.Monad.Writer  hiding (mapM_)
import Data.IORef
import Control.FRPNow.Private.Ref
import Control.FRPNow.Private.PrimEv
import System.IO.Unsafe
import Debug.Trace

import Prelude 

{--------------------------------------------------------------------
  Pure interface
--------------------------------------------------------------------}

-- $time
-- The FRPNow interface is centered around behaviors, values that change over time, and events, value that are known from some point in time on.
--
-- What the pure part of the FRPNow interface does is made precise by denotation semantics, i.e. mathematical meaning. The denotational semantics of the pure interface are
-- 
-- @ 
-- type Event a = (Time+,a)
-- 
-- never :: Event a
-- never = (∞, undefined)
--
-- instance Monad Event where
--   return x = (-∞,x)
--   (ta,a) >>= f = let (tb,b) = f a
--                  in (max ta tb, b)
--
-- type Behavior a = Time -> a 
--
-- instance Monad Behavior where
--   return x = λt -> x
--   m >>= f  = λt -> f (m t) t 
--
-- instance MonadFix Behavior where
--   mfix f = λt -> let x = f x t in x 
-- 
-- switch :: Behavior a -> Event (Behavior a) -> Behavior a
-- switch b (ts,s) = λn -> 
--   if n < ts then b n else s n
--
-- whenJust :: Behavior (Maybe a) -> Behavior (Event a)
-- whenJust b = λt -> 
--   let w = minSet { t' | t' >= t && isJust (b t') }
--   in  if w == ∞ then never
--       else (w, fromJust (b w))
-- @
-- 
-- Where @Time@ is a set that is totally ordered set and has a least element, -∞.
-- For events, we also use @Time+ = Time ∪ ∞@. 
--
-- The notation @minSet x@ indicates the minimum element of the set @x@, which is not valid Haskell, but is a valid denotation. Note that if there is no time at which the input behavior is @Just@ in the present or future, then @minSet@ will give the minimum element of the empty set, which is @∞@.
--   
-- The monad instance of events is denotationally a writer monad in time, whereas the monad instance of behaviors is denotationally a reader monad in time.

-- | An event is a value that is known from some point in time on. 
data Event a  
  = Never
  | Occ a 
  | E (M (Event a))

runE :: Event a -> M (Event a)
runE Never   = return Never
runE (Occ x) = return (Occ x)
runE (E m)   = m 


instance Monad Event where
  return = Occ
  e  >>= f = memoE (e `bindE` f)


-- | A never occuring event

never :: Event a
never = Never

bindE :: Event a -> (a -> Event b) -> Event b
Never   `bindE` _ = Never
(Occ x) `bindE` f = f x
(E m)   `bindE` f = E $ bindEM m f


bindEM :: M (Event a) -> (a -> Event b) -> M (Event b)
m   `bindEM` f = 
    m >>= \r -> case r of
                      Never    -> return Never
                      Occ x    -> runE (f x)
                      E m'     -> return (E $ m' `bindEM` f)
-- Section 6.2


memoEIO :: Event a -> IO (Event a)
memoEIO einit = 
  do r <- newIORef einit 
     return (usePrevE r)

usePrevE :: IORef (Event a) -> Event a
usePrevE r = E $ 
  do e <- liftIO (readIORef r)
     res <- runE e
     liftIO (writeIORef r res)
     return res

memoE :: Event a -> Event a
--memoE e = e
memoE Never = Never
memoE (Occ x) = Occ x
memoE e = unsafePerformIO $ memoEIO e
  
-- Section 6.3

-- | An behavior is a value that changes over time.

data Behavior a = B (M (a, Event (Behavior a)))
                | Const a 


runB :: Behavior a -> M (a, Event (Behavior a))
runB (B m) = m
runB (Const a) = return (a, never)

switch' ::  Behavior a -> Event (Behavior a) -> Behavior a
switch' b Never = b
switch' _ (Occ b) = b
switch' (Const x) (E em) = B $ 
     em >>= \r -> case r of
          Never -> return (x,never)
          Occ b' -> runB b'
          E em'  -> return (x, E em')
switch' (B bm) (E em) = B $
    em >>= \r -> case r of
        Never      -> bm
        Occ   b'   -> runB b'
        E em'      -> 
            do  (h,t) <- bm
                return $ case t of
                  Occ _ -> error "switch already occured!"
                  Never -> (h, E em')
                  E tm  -> (h, switchEM tm em')

switchEM :: M (Event (Behavior a)) -> M (Event (Behavior a)) -> Event (Behavior a)
switchEM lm rm = E $ 
 rm >>= \case 
    Never -> lm
    Occ b -> return (Occ b)
    E rm'    -> lm >>= return . \case
         Never -> E rm'
         Occ b -> Occ (b `switch'` E rm')
         E lm' -> switchEM lm' rm'


bindB :: Behavior a -> (a -> Behavior b) -> Behavior b
bindB (Const x) f = f x
bindB (B m)     f = B $
     do (h,t) <- m
        case f h of
          Const x -> return (x, (`bindB` f) <$> t)
          B n     -> do (hn,tn) <- n
                        tn <- runE tn
                        return $ case (t,tn) of
                          (_, Occ _)  -> error "switch already occured!"
                          (Occ _ , _) -> error "switch already occured!"
                          (Never , e) -> (hn, e)
                          (e, Never ) -> (hn, (`bindB` f) <$> t)
                          (e, E tm) -> (hn, switchEM tm (runE ((`bindB` f) <$> e)) )



whenJust' :: Behavior (Maybe a) -> Behavior (Event a)
whenJust' (Const Nothing)  = pure never
whenJust' (Const (Just x)) = pure (pure x)
whenJust' (B m) = B $ 
    do  (h, t) <- m
        case h of
         Just x -> return (return x, whenJust'  <$> t)
         Nothing -> 
          do  en <- planM (runB . whenJust'  <$> t)
              return (en >>= fst, en >>= snd)


whenJustSample' :: Behavior (Maybe (Behavior a)) -> Behavior (Event a)
whenJustSample' (Const Nothing)  = pure never
whenJustSample' (Const (Just x)) = B $ do v <- fst <$> runB x; return (pure v, never)
whenJustSample' (B bm) = B $
  do (h, t) <- bm
     case h of
      Just x -> do v <- fst <$> runB x; return (pure v, whenJustSample' <$> t)
      Nothing -> do en <- planM (runB . whenJustSample' <$> t)
                    return (en >>= fst, en >>= snd)

instance Monad Behavior where
  return x = B $ return (x, never)
  m >>= f = memoB (m `bindB` f)

instance MonadFix Behavior where
  mfix f = B $ mfix $ \(~(h,_)) ->
       do  ~(h',t) <- runB (f h)
           return (h, mfix f <$ t)

-- | Introduce a change over time.
--
-- 
-- > b `switch` e 
-- 
--
-- Gives a behavior that acts as @b@ initially, and switches to the behavior inside @e@ as soon as @e@ occurs.
--  
switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch b e = memoB (switch' b e)

-- | Observe a change over time.
-- 
-- The behavior @whenJust b@ gives at any point in time the event that 
-- the behavior @b@ is @Just@ at that time or afterwards. 
--
-- As an example,
--
-- 
-- > let getPos x 
-- >         | x > 0 = Just x
-- >         | otherwise = Nothing
-- > in whenJust (getPos <$> b)
-- 
-- Gives gives the event that
-- the behavior @b@ is positive. If @b@ is currently positive
-- then the event will occur now, otherwise it
-- will be the first time that @b@ becomes positive in the future.
-- If @b@ never again is positive then the result is 'never'.

whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust b = memoB (whenJust' b)


-- | A more optimized version of:
-- 
-- > whenJustSample b = do x <- whenJust b 
-- >                       plan x

whenJustSample :: Behavior (Maybe (Behavior a)) -> Behavior (Event a)
whenJustSample b = memoB (whenJustSample' b)


-- | Not typically needed, used for event streams.
--  
-- If we have a behavior giving events, such that each time the behavior is
-- sampled the obtained event is in the future, then this function
-- ensures that we can use the event without inspecting it (i.e. before binding it).
--
-- If the implementation samples such an event and it turns out the event does actually occur at the time
-- the behavior is sampled, an error is thrown.
futuristic :: Behavior (Event a) -> Behavior (Event a)
futuristic b =  B $ do e <- makeLazy (joinEm <$> runB b) 
                       return (fst <$> e, snd <$> e)
  where joinEm (e,es) = (,) <$> e <*> es

unrunB :: (a,Event (Behavior a)) -> Behavior a 
unrunB (h, Never) = Const h
unrunB (h,t) = B $ 
  runE t >>= \x -> case x of
        Occ b -> runB b
        t' -> return (h,t')

memoBIO :: Behavior a -> IO (Behavior a)
memoBIO einit = 
  do r <- newIORef einit 
     return (usePrevB r)

usePrevB :: IORef (Behavior a) -> Behavior a
usePrevB r = B $ 
  do b <- liftIO (readIORef r)
     res <- runB b
     liftIO (writeIORef r (unrunB res))
     return res
     
memoB :: Behavior a -> Behavior a
--memoB b = b
memoB b@(Const _) = b
memoB b = unsafePerformIO $ memoBIO b

-- Section 6.7


data Env = Env {
  plansRef  :: IORef Plans,
  laziesRef :: IORef Lazies,
  clock     :: Clock }



type M = ReaderT Env IO

-- | A monad that alows you to:
-- 
--   * Sample the current value of a behavior via 'sampleNow'
--   * Interact with the outside world via 'async',  'callback' and 'sync'.
--   * Plan to do Now actions later, via 'planNow'
--
-- All actions in the @Now@ monad are conceptually instantaneous, which entails it is guaranteed that for any behavior @b@ and Now action @m@:
-- 
-- @
--    do x <- sample b; m ; y <- sample b; return (x,y) 
-- == do x <- sample b; m ; return (x,x) 
-- @
newtype Now a = Now { getNow :: M a } deriving (Functor,Applicative,Monad, MonadFix)


-- | Sample the present value of a behavior
sampleNow :: Behavior a -> Now a
sampleNow (B m) = Now $ fst <$> m


-- | Create an event that occurs when the callback is called. 
-- 
-- The callback can be safely called from any thread. An error occurs if the callback is called more than once. 
--
-- See 'Control.FRPNow.EvStream.callbackStream' for a callback that can be called repeatidly.
--  
-- The event occurs strictly later than the time that 
-- the callback was created, even if the callback is called immediately.
callback ::  Now (Event a, a -> IO ())
callback = Now $ do c <- clock <$> ask
                    (pe, cb) <- liftIO $ callbackp c
                    return (toE pe,cb)
-- | Synchronously execte an IO action.
-- 
-- Use this is for IO actions which do not take a long time, such as 
-- opening a file or creating a widget.
sync :: IO a -> Now a
sync m = Now $ liftIO m

-- | Asynchronously execte an IO action, and obtain the event that it is done.
-- 
-- Starts a seperate thread for the IO action, and then immediatly returns the 
-- event that the IO action is done. Since all actions in the 'Now' monad are instantaneous,
-- the resulting event is guaranteed to occur in the future (not now).
--
-- Use this for IO actions which might take a long time, such as waiting for a network message,
-- reading a large file, or expensive computations.
--
-- /Note/:Use this only when using FRPNow with Gloss or something else that does not block haskell threads. 
-- For use with GTK or other GUI libraries that do block Haskell threads, use 'asyncOS' instead.
async :: IO a -> Now (Event a)
async m = Now $ do  c <- clock <$> ask
                    toE <$> liftIO (spawn c m)


-- | Like 'async', but uses an OS thread instead of a regular lightweight thread.
--
-- Useful when interacting with GUI systems that claim the main loop, such as GTK.
asyncOS :: IO a -> Now (Event a)
asyncOS m = Now $ do  c <- clock <$> ask
                      toE <$> liftIO (spawnOS c m)

toE :: PrimEv a -> Event a
toE p = E toEM where
  toEM = (toEither . (p `observeAt`) <$> getRound) 
  toEither Nothing   = E toEM
  toEither (Just x)  = Occ x

getRound :: M Round
getRound = ReaderT $ \env -> curRound (clock env)  


-- IORef
type Plan a = IORef (Either (Event (M a)) a)

planToEv :: Plan a -> Event a
planToEv ref = self where
 self = E $ 
  liftIO (readIORef ref) >>= \pstate -> 
  case pstate of
   Right x   -> return (Occ x)
   Left ev   -> runE ev >>= \estate ->
    case estate of
     Occ m  -> do x <- m
                  liftIO $ writeIORef ref (Right x)
                  return $ Occ x
     ev' -> do liftIO $ writeIORef ref (Left ev')
               return self


data SomePlan = forall a. SomePlan (Ref (Plan a))
type Plans = [SomePlan]


type Lazies = [Lazy]
data Lazy = forall a. Lazy (M (Event a)) (IORef (Event a))


makeLazy :: M (Event a) -> M (Event a)
makeLazy m =  ReaderT $ \env ->
       do n <- curRound (clock env)   
          r <- newIORef undefined
          modifyIORef (laziesRef env) (Lazy m r :)
          return (readLazyState n r)

readLazyState :: Round -> IORef (Event a) -> Event a
readLazyState n r =
  let x = E $
       do m <- getRound
          case compare n m of
            LT -> liftIO (readIORef r) >>= runE
            EQ -> return x
            GT -> error "Round seems to decrease.."
  in x


planM :: Event (M a) -> M (Event a)
planM e = plan makeWeakIORef e


-- | Plan to execute a 'Now' computation.
--
-- When given a event carrying a now computation, execute that now computation as soon as the event occurs.
-- If the event has already occured when 'planNow' is called, then the 'Now' computation will be executed immediatly.
planNow :: Event (Now a) -> Now (Event a)
planNow e = Now $ plan makeStrongRef (getNow  <$> e)

plan :: (forall v. IORef v -> IO (Ref (IORef v))) -> Event (M a) -> M (Event a)
plan makeRef e = 
  do p <- liftIO (newIORef $ Left e)
     let ev = planToEv p
     pr <- liftIO (makeRef p)
     addPlan pr
     return ev

addPlan :: Ref (Plan a) -> M ()
addPlan p = ReaderT $ \env -> modifyIORef (plansRef env)  (SomePlan p :) 



-- | General interface to interact with the FRP system. 
--
-- Typically, you don't need this function, but instead use a specialized function for whatever library you want to use FRPNow with such as 'Control.FRPNow.GTK.runNowGTK' or 'Control.FRPNow.Gloss.runNowGloss', which themselves are implemented using this function.

initNow :: 
      (IO (Maybe a) -> IO ()) -- ^ An IO action that schedules some FRP actions to be run. The callee should ensure that all actions that are scheduled are ran on the same thread. If a scheduled action returns @Just x@, then the ending event has occured with value @x@ and now more FRP actions are scheduled.
  ->  Now (Event a) -- ^ The @Now@ computation to execute, resulting in the ending event, i.e. the event that stops the FRP system.
  -> IO ()
initNow schedule (Now m) = 
    mdo c <- newClock (schedule it)
        pr <- newIORef []
        lr <- newIORef []
        let env = Env pr lr c
        let it = runReaderT (iteration e) env
        e <- runReaderT m env
        schedule (runReaderT (iterationMeat e) env)
        return ()

iteration :: Event a -> M (Maybe a)
iteration ev = 
    newRoundM  >>= \new ->
       if new 
       then iterationMeat ev
       else return Nothing

iterationMeat ev = 
  do er <- runE ev
     case er of
       Occ x     -> return (Just x)
       _         -> tryPlans >> runLazies >> return Nothing


newRoundM :: M Bool
newRoundM = ReaderT $ \env -> newRound (clock env)


tryPlans :: M ()
tryPlans = ReaderT $ tryEm where
  tryEm env = 
    do pl <- readIORef (plansRef env)
       --putStrLn ("nr plans: " ++ show (length pl))
       writeIORef (plansRef env) []
       runReaderT (mapM_ tryPlan (reverse pl)) env
  tryPlan (SomePlan pr) = 
   do  ps <-  liftIO (deRef pr) 
       case ps of
        Just p -> do  eres <- runE (planToEv p)
                      case eres of
                       Occ x -> return ()
                       _  -> addPlan pr
        Nothing -> return ()

runLazies :: M ()
runLazies = ReaderT $ runEm where
  runEm env = 
    readIORef (laziesRef env) >>= \pl ->
       if null pl 
       then return ()
       else do writeIORef (laziesRef env) []
               runReaderT (mapM_ runLazy (reverse pl)) env
               runEm env where
  runLazy (Lazy m r) = do e <- m
                          x <- runE e
                          case x of
                            Occ _ -> error "Forced lazy was not lazy!"
                            e'    -> liftIO $ writeIORef r e'


-- | Run the FRP system in master mode.
--
-- Typically, you don't need this function, but instead use a function for whatever library you want to use FRPNow with such as 'Control.FRPNow.GTK.runNowGTK', 'Control.FRPNow.Gloss.runNowGloss'. This function can be used in case you are not interacting with any GUI library, only using FRPNow.
--
-- Runs the given @Now@ computation and the plans it makes until the ending event (given by the inital @Now@ computation) occurs. Returns the value of the ending event.

runNowMaster :: Now (Event a) -> IO a
runNowMaster m = 
   do chan <- newChan
      let enqueue m = writeChan chan m
      initNow enqueue m
      loop chan where
  loop chan = 
      do m <- readChan chan
         mr <- m
         case mr of
           Just x  -> return x
           Nothing -> loop chan



instance Functor Behavior where
  fmap = liftM

instance Applicative Behavior where
  pure = return
  (<*>) = ap
                 
instance Functor Event where
  fmap = liftM

instance Applicative Event where
  pure = return
  (<*>) = ap
