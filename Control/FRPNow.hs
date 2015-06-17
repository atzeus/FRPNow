{-# LANGUAGE RecursiveDo, FlexibleContexts, ExistentialQuantification, Rank2Types,GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.FRPNow
-- Copyright   :  (c) Atze van der Ploeg 2015
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
-- 
-- The core FRPNow interface, based on the paper "Principled Practical FRP: Forget the past, Change the future, FRPNow!", ICFP 2015, by Atze van der Ploeg and Koenem Claessem.
-- 
--  * The pure interface, which has denotational semantics
--  * The IO interface
--  * The entry points, i.e. the functions that are used to start the FRP system.

module Control.FRPNow(
   -- * Pure interface
   -- $time
   Event,Behavior, never, switch, whenJust,
  -- * IO interface
   Now, async, callback, sample, planNow,  sync,
  -- * Entry point
   runNowMaster,
   initNow) where
import Control.Concurrent.Chan
import Control.Applicative hiding (empty)
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Control.Monad.Reader  hiding (mapM_)
import Control.Monad.Writer  hiding (mapM_)
import Data.IORef
import Control.Ref
import Data.Sequence
import System.IO.Unsafe
import Data.Foldable
import Debug.Trace
import Control.PrimEv

import Prelude hiding (mapM_)

{--------------------------------------------------------------------
  Pure interface
--------------------------------------------------------------------}

-- $time
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
-- instance Monad Behavior where
--   return x = λt -> x
--   m >>= f  = λt -> f (m t) t 
-- 
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
-- @Time@ which is totally ordered set and has a least element, -∞.
-- For events, we also use @Time+ = Time ∪ ∞@. 
--
-- The notation @minSet x@ indicates the minimum element of the set @x@, which is not valid Haskell, but is a valid denotation. Note that if there is no time at which the input behavior is @Just@ in the present or future, then @minSet@ will give the minimum element of the empty set, which is @∞@.
--   

-- | An event is a value that is known from some point in time on. Denotationally a writer monad in time.
data Event a  
  = Never
  | Occ a 
  | E { runE :: M (Event a) }


instance Monad Event where
  return = Occ
  Never   >>= _ = Never
  (Occ x) >>= f = f x
  (E m)   >>= f = memoE (m `bindLeakE` f) 

-- | A never occuring event

never = E $ return (Left never)



m `bindLeakE` f = E $ 
    runE m >>= \r -> case r of
                      Right x  ->  runE (f x)
                      Left e'  ->  return (Left (e' >>= f))

minTime :: Event x -> Event y -> Event ()
minTime l r  = E (merge <$> runE l <*> runE r) where
  merge (Right _)  _            = Right ()
  merge _          (Right _  )  = Right ()
  merge (Left l')  (Left r'  )  = Left (minTime l' r')

-- Section 6.2

unrunE :: Either (Event a) a -> Event a 
unrunE (Left e)    = e
unrunE (Right a)   = pure a

memoEIO :: Event a -> IO (Event a)
memoEIO einit = 
  do r <- newIORef einit 
     return (usePrevE r)

usePrevE :: IORef (Event a) -> Event a
usePrevE r = E $ 
  do e <- liftIO (readIORef r)
     res <- runE e
     liftIO (writeIORef r (unrunE res))
     return res

memoE :: Event a -> Event a
--memoE e = e
memoE e = unsafePerformIO $ memoEIO e
  
-- Section 6.3

-- | An behavior is a value that changes over time. Denotationally a reader monad in time.

data Behavior a = B { runB :: M (a, Event (Behavior a)) }

switchLeak ::  Behavior a -> Event (Behavior a) -> Behavior a
switchLeak b e     = B $
    runE e >>= \r -> case r of
        Right x   -> runB x
        Left  e'  -> do  (h,t) <- runB b
                         return (h, switchE t e')

switchE :: Event (Behavior a) -> Event (Behavior a) -> Event (Behavior a)
switchE l r =  ((pure undefined `switchLeak` l) `switchLeak` r) <$ 
               minTime l r

joinBLeak :: Behavior (Behavior a) -> Behavior a
joinBLeak m = B $ 
    do  (h,t) <- runB m
        runB $ h `switchLeak` (joinBLeak <$> t)

fmapLeak f (B b) = B $ 
       do (h,t) <- b
          return (f h, fmap (fmap f) t)

bindLeakB :: Behavior a -> (a -> Behavior b) -> Behavior b
m `bindLeakB` f = joinBLeak (fmap f m)

whenJustLeak :: Behavior (Maybe a) -> Behavior (Event a)
whenJustLeak b = B $ 
    do  (h, t) <- runB b
        case h of
         Just x -> return (return x, whenJustLeak <$> t)
         Nothing -> 
          do  en <- planM (runB . whenJustLeak <$> t)
              return (en >>= fst, en >>= snd)

instance Functor Behavior where
  fmap f b = memoB (fmapLeak f b)

instance Monad Behavior where
  return x = B $ return (x, never)
  m >>= f = memoB (m `bindLeakB` f)

-- | Introduce a change over time.
--
-- 
-- > b `switch` e 
-- 
--
-- Gives a behavior that acts as @b@ initially, and switches to the behavior inside @e@ as soon as @e@ occurs.
--  
switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch b e = memoB (switchLeak b e)

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
whenJust b = memoB (whenJustLeak b)



-- Section 6.4

unrunB :: (a,Event (Behavior a)) -> Behavior a 
unrunB (h,t) = B $ 
  runE t >>= \x -> case x of
        Right b -> runB b
        Left t' -> return (h,t')

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
memoB b = unsafePerformIO $ memoBIO b

-- Section 6.7


data Env = Env {
  plansRef :: IORef Plans,
  clock    :: Clock }

type M = ReaderT Env IO

-- | A monad that alows you to:
-- 
--   * Sample the current value of a behavior via 'sample'
--   * Interact with the outside world via 'async', 'callback' and 'sync'.
--   * Plan to do Now actions later, via 'planNow'
--
-- All actions in the @Now@ monad are conceptually instantaneous, which entails it is guaranteed that:
-- 
-- @
--    do x <- sample b; y <- sample b; return (x,y) 
-- == do x <- sample b; return (x,x) 
-- @
newtype Now a = Now { getNow :: M a } deriving (Functor,Applicative,Monad)


-- | Sample the present value of a behavior
sample :: Behavior a -> Now a
sample (B m) = Now $ fst <$> m


-- | Create an event that occurs when the callback is called.
-- 
-- The event occurs strictly later than the time that 
-- the callback was created, even if the callback is called immediately.
callback ::  Now (Event a, a -> IO ())
callback = Now $ do c <- clock <$> ask
                    (pe, cb) <- liftIO $ callbackp c
                    return (toE pe,cb)
-- | Synchronously execte an IO action.
-- 
-- Use this is for IO actions which take little time, such as 
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
async :: IO a -> Now (Event a)
async m = Now $ do  c <- clock <$> ask
                    toE <$> liftIO (spawn c m)

toE :: PrimEv a -> Event a
toE p = E (toEither . (p `observeAt`) <$> getRound) 
  where  toEither Nothing   = Left (toE p)
         toEither (Just x)  = Right x
getRound :: M Round
getRound = ReaderT $ \env -> curRound (clock env)  

data Plan a = Plan (Event (M a)) (IORef (Maybe a))

planToEv :: Plan a -> Event a
planToEv (Plan ev ref) = E $
  liftIO (readIORef ref) >>= \pstate -> 
  case pstate of
   Just x   -> return (Right x)
   Nothing  -> runE ev >>= \estate ->
    case estate of
     Left ev' -> 
         return $ Left $ planToEv (Plan ev' ref)
     Right m  -> do  v <- m
                     liftIO $ writeIORef ref (Just v)
                     return $ Right v

data SomePlan = forall a. SomePlan (Ref (Plan a))
type Plans = Seq SomePlan

planM :: Event (M a) -> M (Event a)
planM e = plan makeWeakRef e


-- | Plan to execute a 'Now' computation.
--
-- When given a event carrying a now computation, execute that now computation as soon as the event occurs.
-- If the event has already occured when 'planNow' is called, then the 'Now' computation will be executed immediatly.
planNow :: Event (Now a) -> Now (Event a)
planNow e = Now $ plan makeStrongRef (getNow  <$> e)

plan :: (forall x. x -> IO (Ref x)) -> Event (M a) -> M (Event a)
plan makeRef e = 
  do p <- Plan e <$> liftIO (newIORef Nothing)
     pr <- liftIO (makeRef p)
     addPlan pr
     return (planToEv p)

addPlan :: Ref (Plan a) -> M ()
addPlan p = ReaderT $ \env -> modifyIORef (plansRef env)  (SomePlan p <|) 



-- | General interface to interact with the FRP system. 
--
-- Typically, you don't need this function, but instead use a specialized function for whatever library you want to use FRPNow with such as 'runNowGTK', 'runNowGloss'. These functions themselves are implemented using this function.
--
-- Parameters:
-- 
--   [@IO (Maybe a) -> IO ()@] An IO action that schedules some FRP actions to be run. The callee should ensure that all actions that are scheduled are ran on the same thread. If a scheduled action returns @Just x@, then the ending event has occured with value @x@ and now more FRP actions are scheduled.
--               
--   [@Now (Event a)@] The @Now@ computation to execute, resulting in the ending event, i.e. the event that stops the FRP system.


initNow :: (IO (Maybe a) -> IO ()) ->  Now (Event a) -> IO ()
initNow schedule (Now m) = 
    mdo c <- newClock (schedule it)
        pr <- newIORef empty
        let env = Env pr c
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
       Right x   -> return (Just x)
       Left _    -> tryPlans >> return Nothing


newRoundM :: M Bool
newRoundM = ReaderT $ \env -> newRound (clock env)


tryPlans :: M ()
tryPlans = ReaderT $ tryEm where
  tryEm env = 
    do pl <- readIORef (plansRef env)
       writeIORef (plansRef env) empty
       runReaderT (mapM_ tryPlan pl) env
  tryPlan (SomePlan pr) = 
   do  ps <-  liftIO (deRef pr) 
       case ps of
        Just p -> do  eres <- runE (planToEv p)
                      case eres of
                       Right x -> return ()
                       Left _  -> addPlan pr
        Nothing -> return ()



-- | Run the FRP system in master mode.
--
-- Typically, you don't need this function, but instead use a function for whatever library you want to use FRPNow with such as 'runNowGTK', 'runNowGloss'. This function can be used in case you are not interacting with any library that claims the main loop.
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

instance Applicative Behavior where
  pure = return
  (<*>) = ap
                 
instance Functor Event where
  fmap = liftM

instance Applicative Event where
  pure = return
  (<*>) = ap
