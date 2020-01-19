{-# LANGUAGE DeriveDataTypeable, RecursiveDo, FlexibleContexts, ExistentialQuantification, Rank2Types,GeneralizedNewtypeDeriving  #-}
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
import Control.Exception
import Data.Typeable
import Control.Applicative hiding (empty,Const)
import Control.Monad hiding (mapM_, fail)
import Control.Monad.IO.Class
import Control.Monad.Reader  hiding (mapM_, fail)
import Control.Monad.Writer  hiding (mapM_, fail)
import Data.IORef
import Control.FRPNow.Private.Ref
import Control.FRPNow.Private.PrimEv
import System.IO.Unsafe
import Debug.Trace
import Control.Monad.Fail

import Prelude hiding (fail)

{--------------------------------------------------------------------
  Pure interface
--------------------------------------------------------------------}

-- $time
-- The FRPNow interface is centered around behaviors, values that change over time, and events, values that are known from some point in time on.
--
-- What the pure part of the FRPNow interface does is made precise by denotational semantics, i.e. mathematical meaning. The denotational semantics of the pure interface are
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

newtype EInternal a = EInternal { runEInternal :: M (Either (EInternal a) (Event a)) }

data State = Update
           | Redirect

runE :: Event a -> M (Event a)
runE Never   = return Never
runE (Occ x) = return (Occ x)
runE (E m)   = m


instance Monad Event where
  return = Occ
  Never >>= _ = Never
  (Occ x) >>= f = f x
  (E m)   >>= f = memoE $ bindInternal m f


-- | A never occurring event

never :: Event a
never = Never


setE :: a -> Event x -> Event a
setE _ Never = Never
setE a (Occ _) = Occ a
setE a (E m) = E $ setE a <$> m

bindInternal :: M (Event a) -> (a -> Event b) -> EInternal b
m   `bindInternal` f = EInternal $
    m >>= \r -> case r of
                      Never    -> return (Right Never)
                      Occ x    -> Right <$> runE (f x)
                      E m'     -> return (Left $ m' `bindInternal` f)

minTime Never r = setE () r
minTime l Never = setE () l
minTime (Occ _) _ = Occ ()
minTime _ (Occ _) = Occ ()
minTime (E ml) (E mr) = memoE $ minInternal ml mr

minInternal :: M (Event a) -> M (Event b) -> EInternal ()
minInternal ml mr = EInternal $
  do er <- mr
     case er of
      Occ x -> return (Right (Occ ()))
      Never -> return (Right (setE () $ E ml))
      E mr' -> do el <- ml
                  return $ case el of
                    Occ x ->  Right (Occ ())
                    Never -> Right (setE () $ E mr')
                    E ml' -> Left (minInternal ml' mr')



memoEIO :: EInternal a -> IO (Event a)
memoEIO einit =
  do r <- newIORef (Left einit,Nothing )
     return (usePrevE r)

usePrevE :: IORef (Either (EInternal a) (Event a), (Maybe (Round, Event a))) -> Event a
usePrevE r = self where
 self = E $
  do (s,cached) <- liftIO (readIORef r)
     round <- getRound
     case cached of
        Just (cr,cache) | cr == round -> return cache
        _ -> case s of
              Left ei -> do ri <- runEInternal ei
                            case ri of
                             Left _  -> do liftIO (writeIORef r (ri,Just (round,self) ) )
                                           return self
                             Right e -> do liftIO (writeIORef r (ri, Just (round,e)) )
                                           return e
              Right e -> do e' <- runE e
                            liftIO (writeIORef r (Right e', Just (round,e')))
                            return e'

memoE :: EInternal a -> Event a
--memoE e = e
memoE e = unsafePerformIO $ memoEIO e

-- Section 6.3

-- | A behavior is a value that changes over time.

data Behavior a = B (M (a, Event (Behavior a)))
                | Const a

data BInternal a = BInternal { runBInternal ::  M (Either (BInternal a, a, Event ()) (Behavior a)) }


memoBIIO :: BInternal a -> IO (Behavior a)
memoBIIO einit =
  do r <- newIORef (Left einit, Nothing)
     return (usePrevBI r)

usePrevBI :: IORef (Either (BInternal a) (Behavior a), Maybe (a, Event (Behavior a)) ) -> Behavior a
usePrevBI r = self where
 self = B $
  do (s,cached) <- liftIO (readIORef r)
     case cached of
      Just (cache@(i,ev)) ->
       do ev' <- runE ev
          case ev' of
           Occ x -> update s
           _     -> do liftIO (writeIORef r (s, Just (i,ev')))
                       return (i,ev')
      Nothing -> update s
 update s = case s of
             Left ei -> do ri <- runBInternal ei
                           case ri of
                            Left (bi',i,e) ->
                                   do let res = (i, setE self e)
                                      liftIO (writeIORef r (Left bi',Just res))
                                      return res
                            Right b -> do res@(h,t) <- runB b
                                          liftIO (writeIORef r (Right (rerunBh res), Just res))
                                          return res
             Right b -> do res@(h,t) <- runB b
                           liftIO (writeIORef r (Right (rerunBh res), Just res))
                           return res

memoBInt :: BInternal a -> Behavior a
--memoE e = e
memoBInt e = unsafePerformIO $ memoBIIO e

runB :: Behavior a -> M (a, Event (Behavior a))
runB (B m) = m
runB (Const a) = return (a, never)

rerunBh :: (a,Event(Behavior a)) -> Behavior a
rerunBh (h,Never) = Const h
rerunBh (h,t) = B $ runE t >>= \x -> case x of
        Occ b -> runB b
        t' -> return (h,t')

rerunB :: a -> Event (Behavior a)  -> M (a, Event (Behavior a))
rerunB h  Never = return (h, Never)
rerunB h t      = runE t >>= \x -> case x of
        Occ b -> runB b
        t' -> return (h,t')


switchInternal :: M (a, Event (Behavior a)) -> M (Event (Behavior a)) -> BInternal a
switchInternal mb me = BInternal $
    do e <- me
       case e of
        Occ x -> return (Right x)
        Never -> return (Right (B mb))
        E me' -> do (i,ei) <- mb
                    return $ Left (switchInternal (rerunB i ei) me', i, minTime ei e)

stepInternal :: a -> M (Event (Behavior a)) -> BInternal a
stepInternal i me =BInternal $
    do e <- me
       return $ case e of
        Occ x -> Right x
        Never -> Right (Const i)
        E me' -> Left (stepInternal i me', i, setE () e)

bindBInternal :: M (a,Event (Behavior a)) -> (a -> Behavior b) -> BInternal b
bindBInternal m f =
 BInternal $
  do (h,t) <- m
     case t of
      Never -> return $ Right (f h)
      Occ _ -> error "invariant broken"
      _     ->
       case f h of
        Const x -> return $ Left (bindBInternal (rerunB h t) f, x, setE () t)
        B n     -> do (hn,tn) <- n
                      return $ Left (bindBInternal (rerunB h t) f, hn, minTime t tn)



bindB :: Behavior a -> (a -> Behavior b) -> Behavior b
bindB (Const x) f = f x
bindB (B m)     f = memoBInt $ bindBInternal m f

whenJustInternal :: M (Maybe a, Event (Behavior (Maybe a))) -> Behavior (Event a) -> BInternal (Event a)
whenJustInternal m outerSelf = BInternal $
    do (h, t) <- m
       case t of
        Never -> return $ Right $ pure $ case h of
                            Just x -> pure x
                            Nothing -> never
        Occ _ -> error "invariant broken"
        _     ->
         case h of
          Just x -> return $ Left (whenJustInternal (rerunB h t) outerSelf, return x, setE () t)
          Nothing ->
           do  en <- planM (setE (runB outerSelf) t)
               return $ Left (whenJustInternal (rerunB h t) outerSelf, en >>= fst, setE () t)


whenJust' :: Behavior (Maybe a) -> Behavior (Event a)
whenJust' (Const Nothing)  = pure never
whenJust' (Const (Just x)) = pure (pure x)
whenJust' (B m) =  let x =  memoBInt $ whenJustInternal m x
                   in x


{-
whenJustSample' :: Behavior (Maybe (Behavior a)) -> Behavior (Event a)
whenJustSample' (Const Nothing)  = pure never
whenJustSample' (Const (Just x)) = B $ do v <- fst <$> runB x; return (pure v, never)
whenJustSample' (B bm) = B $
  do (h, t) <- bm
     case h of
      Just x -> do v <- fst <$> runB x; return (pure v, whenJustSample' <$> t)
      Nothing -> do en <- planM (runB . whenJustSample' <$> t)
                    return (en >>= fst, never)
-}
instance Monad Behavior where
  return x = B $ return (x, never)
  m >>= f = m `bindB` f

instance MonadFix Behavior where
  mfix f = B $ mfix $ \(~(h,_)) ->
       do  (h',t) <- runB (f h)
           return (h', mfix f <$ t )

-- | Introduce a change over time.
--
--
-- > b `switch` e
--
--
-- Gives a behavior that acts as @b@ initially, and switches to the behavior inside @e@ as soon as @e@ occurs.
--
switch ::  Behavior a -> Event (Behavior a) -> Behavior a
switch b Never = b
switch _ (Occ b) = b
switch (Const x) (E em) = memoBInt (stepInternal x em)
switch (B bm) (E em) = memoBInt (switchInternal bm em)
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
whenJust b = (whenJust' b)

{-
-- | A more optimized version of:
--
-- > whenJustSample b = do x <- whenJust b
-- >                       plan x

whenJustSample :: Behavior (Maybe (Behavior a)) -> Behavior (Event a)
whenJustSample b = memoB (whenJustSample' b)
-}

-- | Not typically needed, used for event streams.
--
-- If we have a behavior giving events, such that each time the behavior is
-- sampled the obtained event is in the future, then this function
-- ensures that we can use the event without inspecting it (i.e. before binding it).
--
-- If the implementation samples such an event and it turns out the event does actually occur at the time
-- the behavior is sampled, an error is thrown.
futuristic :: Behavior (Event a) -> Behavior (Event a)
futuristic b =  B $ do e <- makeLazy $  fst <$>  runB b
                       return (e,futuristic b <$ e)

unrunB :: (a,Event (Behavior a)) -> Behavior a
unrunB (h, Never) = Const h
unrunB (h,t) = B $
  runE t >>= \x -> case x of
        Occ b -> runB b
        t' -> return (h,t')
{-
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
-}
-- Section 6.7


data Env = Env {
  plansRef  :: IORef Plans,
  laziesRef :: IORef Lazies,
  clock     :: Clock }



type M = ReaderT Env IO

-- | A monad that allows you to:
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
newtype Now a = Now { getNow :: M a } deriving (Functor,Applicative,Monad, MonadFix, MonadIO)

instance MonadFail Now where
  fail = Now . fail

-- | Sample the present value of a behavior
sampleNow :: Behavior a -> Now a
sampleNow (B m) = Now $ fst <$> m


-- | Create an event that occurs when the callback is called.
--
-- The callback can be safely called from any thread. An error occurs if the callback is called more than once.
--
-- See 'Control.FRPNow.EvStream.callbackStream' for a callback that can be called repeatedly.
--
-- The event occurs strictly later than the time that
-- the callback was created, even if the callback is called immediately.
callback ::  Now (Event a, a -> IO ())
callback = Now $ do c <- clock <$> ask
                    (pe, cb) <- liftIO $ callbackp c
                    return (toE pe,cb)
-- | Synchronously execute an IO action.
--
-- Use this is for IO actions which do not take a long time, such as
-- opening a file or creating a widget.
sync :: IO a -> Now a
sync m = Now $ liftIO m

-- | Asynchronously execute an IO action, and obtain the event that it is done.
--
-- Starts a separate thread for the IO action, and then immediatly returns the
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
          r <- newIORef (error "should not have read lazy yet")
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
-- If the event has already occurred when 'planNow' is called, then the 'Now' computation will be executed immediately.
planNow :: Event (Now a) -> Now (Event a)
planNow e = Now $
  do e' <- runE e
     case e' of
      Occ x -> pure <$> getNow x
      Never -> return Never
      _     -> plan makeStrongRef (getNow  <$> e)

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
      (IO (Maybe a) -> IO ()) -- ^ An IO action that schedules some FRP actions to be run. The callee should ensure that all actions that are scheduled are ran on the same thread. If a scheduled action returns @Just x@, then the ending event has occurred with value @x@ and now more FRP actions are scheduled.
  ->  Now (Event a) -- ^ The @Now@ computation to execute, resulting in the ending event, i.e. the event that stops the FRP system.
  -> IO ()
initNow schedule (Now m) =
    mdo c <- newClock (schedule it)
        pr <- newIORef []
        lr <- newIORef []
        let env = Env pr lr c
        let it = runReaderT (iteration e) env
        e <- runReaderT m env
        runReaderT (iterationMeat e) env
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
   do  -- liftIO (traceIO "plan!")
       ps <-  liftIO (deRef pr)
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

-- | When using the FRP system in master mode, with 'runNowMaster', this exception is thrown if
-- the FRP system is not doing anything anymore, waiting for 'never'.

data FRPWaitsForNeverException = FRPWaitsForNeverException deriving (Show, Typeable)

instance Exception FRPWaitsForNeverException

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
      do m <-   catch (readChan chan)
                  (\e -> do let err = (e :: BlockedIndefinitelyOnMVar)
                            throw FRPWaitsForNeverException)
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

