{-# LANGUAGE DataKinds,GADTs,GeneralizedNewtypeDeriving #-}
module Base.Time(PastTime, silentBlockTime, pastTimeAt, bigBang, Time,newPrimitiveTime, timeOf, timeIsNow, minTime,maxTime, Present,doSync, isNow, asap, runPresent)  where

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
import qualified Data.DList as DL
import Data.Maybe


{- interface:

maxtime mintime
newTime
timeIsNow
silentBlockTime
doSync :: IO a -> Present a
isNow  :: Time -> Present Bool
asap   :: Present () -> Time -> Present ()
runPresent :: Present () -> IO ()
-}



bigBang :: PastTime
bigBang = PastTime 0

minTime l r = unsafePerformIO $ minTimeIO l r
{-# NOINLINE minTime #-}

maxTime l r = unsafePerformIO $ maxTimeIO l r
{-# NOINLINE maxTime #-}

--- Globals --

globalClock :: MVar Integer
globalClock = unsafePerformIO $ newMVar 1
{-# NOINLINE globalClock #-}

globalStrongRefs :: IORef (Map Unique Time)
globalStrongRefs =  unsafePerformIO $ newIORef empty
{-# NOINLINE globalStrongRefs #-}

--- Present monad and strong refs

addGlobalStrongRef :: Time -> IO ()
addGlobalStrongRef t = modifyIORef' globalStrongRefs (insert (timeId t) t)

removeGlobalStrongRef :: Time -> IO ()
removeGlobalStrongRef t = modifyIORef' globalStrongRefs (delete (timeId t))

newtype Present a = Present (IO a) deriving (Monad,Applicative,Functor)

doSync :: IO a -> Present a
doSync = Present

isNow :: Time -> Present Bool
isNow t = Present (isJust <$> readIVar (val t))

asap :: Time -> Present () -> Present ()
asap t p = do v <- isNow t 
              if v 
              then p
              else Present $ 
                    do tp <- newTime (RunPresent p)
                       addListener tp (listeners t)
                       addGlobalStrongRef tp


runPresent :: Present () -> IO ()
runPresent (Present p) =
   do tl <- takeMVar globalClock
      p
      putMVar globalClock tl
                      

-- Time types -----

newtype PastTime = PastTime Integer deriving (Ord,Eq)

newtype PrimTime = PrimTime Time

timeOf (PrimTime t) = t

type Listeners = Map Unique (Weak Time)

data TimeExp 
  = Primitive
  | SameAs Time 
  | Min Time Time
  | Max Time Time
  | RunPresent (Present ())
  | Never

data Time = Time { 
    timeId     :: Unique, 
    timeExp    :: IORef (Maybe TimeExp), 
    listeners  :: IORef Listeners,
    val        :: IVar PastTime }

instance Bounded Time where
  minBound = unsafePerformIO $ newDoneTime bigBang
  maxBound = unsafePerformIO $ newTime Never

silentBlockTime :: Time -> PastTime
silentBlockTime t = silentBlockVal (val t)

pastTimeAt :: Time -> PastTime -> Maybe PastTime
pastTimeAt t p = unsafePerformIO $
  do v <- readIVar (val t)
     case v of
      Just tv | tv <= p -> return (Just tv)
      _ -> return Nothing
{-# NOINLINE pastTimeAt #-}


------- Initialization ----------

newPrimitiveTime :: IO PrimTime
newPrimitiveTime = PrimTime <$> newTime Primitive

newTime :: TimeExp -> IO Time
newTime e = Time <$> newUnique <*> newIORef (Just e) <*> newIORef empty <*> newIVar

newDoneTime :: PastTime -> IO Time
newDoneTime p = Time <$> newUnique <*> newIORef Nothing <*> newIORef empty <*> newDoneIVar p

minTimeIO :: Time -> Time -> IO Time
minTimeIO l r = 
   do lv <- readIVar (val l) 
      rv <- readIVar (val r)
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
   do lv <- readIVar (val l) 
      rv <- readIVar (val r)
      case (lv,rv) of
       (Just x, Just y) -> newDoneTime (max x y)
       (Just x, _     ) -> return r
       (_     , Just y) -> return l
       _                -> do t <- newTime (Max l r)
                              addListener t (listeners l)
                              addListener t (listeners r)
                              return t

addListener :: Time -> IORef Listeners -> IO ()
addListener t r = do tw <- mkWeak (val t) t (Just $ removeListener (timeId t) r)
                     modifyIORef' r (insert (timeId t) tw)

removeListener :: Unique -> IORef Listeners -> IO ()
removeListener u r = modifyIORef' r (delete u) 

--------- Update ------------------

timeIsNow :: PrimTime -> IO ()
timeIsNow (PrimTime t) =   
  do tl <- takeMVar globalClock
     runUpdate (PastTime tl) t
     putMVar globalClock (tl + 1) where
  runUpdate time t = setTime t >>= runPresents where

    setTime ::  Time -> IO (DL.DList (Present ())) 
    setTime t = 
      do writeIORef (timeExp t) Nothing
         writeIVar (val t) time
         l <- readIORef (listeners t) 
         writeIORef (listeners t) empty 
         runListeners l

    runListeners :: Listeners -> IO (DL.DList (Present ())) 
    runListeners m = DL.concat <$> mapM runListener (elems m) where
        runListener w = deRefWeak w >>= maybe (return DL.empty) updateTime' 

    updateTime' :: Time -> IO (DL.DList  (Present ())) 
    updateTime' t = readIORef (timeExp t) >>= maybe (error "update to already done time!") (updateTime t)

    updateTime :: Time -> TimeExp -> IO (DL.DList  (Present ())) 
    updateTime t exp = case exp of
        Primitive -> error "updating primitive"
        SameAs x  -> readIVar (val x) >>= maybe (error "useless sameas update!") (const (setTime t))
        Min l r   -> do lv <- readIVar (val l)
                        rv <- readIVar (val r)
                        case (lv,rv) of
                         (Just _ , Just _) -> return ()
                         (Just _ , _     ) -> removeListener (timeId t) (listeners r) 
                         (_      , Just _) -> removeListener (timeId t) (listeners l) 
                         _                 -> error "useless min update!"
                        setTime t 
        Max l r  ->  do lv <- readIVar (val l)
                        rv <- readIVar (val r)
                        case (lv,rv) of
                         (Just _ , Just _) -> setTime t 
                         (Just _ , _     ) -> writeIORef (timeExp t) (Just $ SameAs r) >> return DL.empty
                         (_      , Just _) -> writeIORef (timeExp t) (Just $ SameAs l) >> return DL.empty
                         _                 -> error "useless max update!"
        RunPresent m -> removeGlobalStrongRef t >> return (DL.singleton m)

    runPresents dl = mapM start (DL.toList dl) >>= mapM takeMVar
    start (Present p) = do m <- newEmptyMVar; forkIO (p >> putMVar m ()) ; return m


-------------------------







