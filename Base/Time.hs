{-# LANGUAGE DataKinds,GADTs #-}
module Base.Time(PastTime, bigBang, getTime, Time,newPrimitiveTime, timeOf, timeIsNow, minTime,maxTime, pastTimeAt, waitFor)  where

import System.IO.Unsafe
import Base.IVar
import Control.Monad
import Control.Concurrent.MVar
import Data.IORef
import System.Mem.Weak
import Data.Map
import Control.Applicative hiding (empty)
import Data.Unique
import System.Clock hiding (getTime)
import qualified System.Clock as Clock
import System.IO.Unsafe
import Control.Concurrent
import Base.TryReadMVar
import Debug.Trace
munit = return ()



data PastTime = BigBang 
              | PastTime TimeSpec deriving (Ord,Eq,Show) -- abstract

bigBang :: PastTime
bigBang = BigBang

getTime = 
  do tl <- takeMVar globalClockLowerbound
     t <- higherTime tl
     putMVar globalClockLowerbound t  
     return (PastTime t)


             
higherTime :: TimeSpec -> IO TimeSpec
higherTime tl = loop where
         loop = do t <- Clock.getTime Monotonic
                   if tl == t
                   then yield >> loop
                   else return t


globalClockLowerbound :: MVar TimeSpec
globalClockLowerbound = unsafePerformIO $ newMVar (TimeSpec minBound minBound)
{-# NOINLINE globalClockLowerbound #-}

minTime l r = unsafePerformIO $ minTimeIO l r
{-# NOINLINE minTime #-}

maxTime l r = unsafePerformIO $ maxTimeIO l r
{-# NOINLINE maxTime #-}

type Listeners = Map Unique (Weak Time)

data TimeExp 
  = Primitive
  | SameAs Time 
  | Min Time Time
  | Max Time Time
  | Never



data Time = Time { 
    timeId     :: Unique, 
    timeExp    :: IORef (Maybe TimeExp), 
    listeners  :: IORef Listeners,
    val        :: MVar PastTime }

instance Bounded Time where
  minBound = unsafePerformIO $ newDoneTime bigBang
  maxBound = unsafePerformIO $ newTime Never



newtype PrimTime = PrimTime Time

timeOf (PrimTime t) = t

newPrimitiveTime :: IO PrimTime
newPrimitiveTime = PrimTime <$> newTime Primitive

timeIsNow :: PrimTime-> IO ()
timeIsNow (PrimTime t) =   
  do tl <- takeMVar globalClockLowerbound
     tm <- higherTime tl
     setTime t (PastTime tm)
     putMVar globalClockLowerbound tm  

newTime :: TimeExp -> IO Time
newTime e = Time <$> newUnique <*> newIORef (Just e) <*> newIORef empty <*> newEmptyMVar

newDoneTime :: PastTime -> IO Time
newDoneTime p = Time <$> newUnique <*> newIORef Nothing <*> newIORef empty <*> newMVar p


minTimeIO :: Time -> Time -> IO Time
minTimeIO l r = 
   do lv <- tryReadMVar (val l) 
      rv <- tryReadMVar (val r)
      case (lv,rv) of
       (Just x, Just y) -> newDoneTime (min x y)
       (Just x, _     ) -> newDoneTime x
       (_     , Just y) -> newDoneTime y
       _                -> do t <- newTime (Min l r)
                              addListener t (listeners l)
                              addListener t (listeners r)
                              return t

maxTimeIO :: Time -> Time -> IO Time
maxTimeIO l r = 
   do lv <- tryReadMVar (val l) 
      rv <- tryReadMVar (val r)
      case (lv,rv) of
       (Just x, Just y) -> newDoneTime (max x y)
       (Just x, _     ) -> return r
       (_     , Just y) -> return l
       _                -> do t <- newTime (Max l r)
                              addListener t (listeners l)
                              addListener t (listeners r)
                              return t

runListeners :: Listeners -> IO ()
runListeners m = mapM_ runListener $ elems m where
  runListener w = deRefWeak w >>= maybe munit updateTime 

setTime ::  Time -> PastTime -> IO ()
setTime t p = 
   do v <- readIORef (timeExp t) 
      case v of 
       Nothing -> return ()
       Just _  -> 
              do writeIORef (timeExp t) Nothing
                 putMVar (val t) p
                 l <- readIORef (listeners t) 
                 writeIORef (listeners t) empty 
                 runListeners l


maybeGetTime :: Time -> IO (Maybe PastTime)
maybeGetTime t = tryReadMVar (val t)

updateTime :: Time -> IO ()
updateTime t = 
  do v <- readIORef (timeExp t)
     case v of
      Nothing -> return ()
      Just x -> case x of
        Primitive -> return ()
        SameAs x  -> maybeGetTime x >>= maybe munit (setTime t)
        Min l r   -> do lv <- maybeGetTime l
                        rv <- maybeGetTime r
                        case (lv,rv) of
                         (Just tl, Just tr) -> setTime t (min tl tr)
                         (Just tl, _      ) -> removeListener (timeId t) (listeners r) >>  setTime t tl
                         (_      , Just tr) -> removeListener (timeId t) (listeners l) >>  setTime t tr
                         _                  -> return ()
        Max l r  ->  do lv <- maybeGetTime l
                        rv <- maybeGetTime r
                        case (lv,rv) of
                         (Just tl, Just tr) -> setTime t (max tl tr)
                         (Just tl, _      ) -> writeIORef (timeExp t) (Just $ SameAs r)
                         (_      , Just tr) -> writeIORef (timeExp t) (Just $ SameAs l)
                         _                  -> return ()
        Never -> return ()

addListener :: Time -> IORef Listeners -> IO ()
addListener t r = do tw <- mkWeak (val t) t (Just $ putStrLn "Removing!" >> removeListener (timeId t) r)
                     modifyIORef' r (insert (timeId t) tw)

removeListener :: Unique -> IORef Listeners -> IO ()
removeListener u r = modifyIORef' r (delete u) 

pastTimeAt :: Time -> PastTime -> Maybe PastTime
pastTimeAt t p = unsafePerformIO $
  do v <- tryReadMVar (val t)
     case v of
      Just tv | tv <= p -> return (Just tv)
      _ -> return Nothing
{-# NOINLINE pastTimeAt #-}

waitFor :: Time -> IO PastTime
waitFor t = do v <- readIORef (timeExp t)
               case v of
                 Just Never -> putStrLn "Thread waiting on never!"
                 _          -> return ()
               t <- readMVar (val t) 
               readMVar globalClockLowerbound -- wait for current time to be done
               return t




{-

newtype Future = Future (IVar PastTime) (IORef Listeners))

newFuture :: IO (Future a)
newFuture = liftM Future $ newIORef (Left empty)

futureIsNow :: Future a -> a -> IO ()
futureIsNow (Future r) a = withCurTime (\t -> writeIVar r (t,a)) 

futureInfoAt :: Future a -> PastTime -> Maybe (PastTime,a)
futureInfoAt (Future r) t = unsafePerformIO $
  do v <- readIVar r
     case v of
       Just (tp,a) | tp <= t -> return (Just (tp,a))
       _                     -> return Nothing

type Listeners = Map Unique (Weak Time)
-}



