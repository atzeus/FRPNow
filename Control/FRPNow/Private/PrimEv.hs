module Control.FRPNow.Private.PrimEv(Round, Clock, PrimEv, newClock , callbackp, spawn, spawnOS, curRound, newRound ,observeAt ) where

import Control.Applicative
import System.IO.Unsafe
import Data.IORef
import Data.Unique
import Control.Concurrent
import Debug.Trace

data Clock    = Clock {
   identClock         :: Unique,
   scheduleRound      :: IO (),
   roundRef           :: IORef Integer,
   changedRef         :: IORef Bool }

data Round    = Round Unique Integer
data PrimEv a = PrimEv Unique (IORef (Maybe (Round, a)))

instance Show Round where
  show (Round _ i) = show i



-- when given a IO action that schedules a round, create a new clock
newClock :: IO () -> IO Clock
newClock schedule = Clock <$> newUnique <*> pure schedule <*> newIORef 0 <*> newIORef False

callbackp :: Clock -> IO (PrimEv a, a -> IO ())
callbackp c =
  do mv <- newIORef Nothing
     return (PrimEv (identClock c) mv, setValue mv)
 where setValue mv x =
         do i <- readIORef (roundRef c)
            v <- readIORef mv
            case v of
              Just _ -> error "Already called callback!"
              _      -> return ()
            writeIORef mv (Just (Round (identClock c) (i + 1), x))
            writeIORef (changedRef c) True
            scheduleRound c

spawn :: Clock -> IO a ->  IO (PrimEv a)
spawn c m =
  do (pe,setVal) <- callbackp c
     forkIO $ m >>= setVal 
     return pe

spawnOS :: Clock -> IO a ->  IO (PrimEv a)
spawnOS c m =
  do (pe,setVal) <- callbackp c
     forkOS $ m >>= setVal 
     return pe

curRound :: Clock -> IO Round
curRound c = Round (identClock c) <$> readIORef (roundRef c)

newRound :: Clock -> IO Bool
newRound c =
    readIORef (changedRef c) >>= \change ->
      if change 
      then do  writeIORef (changedRef c) False
               modifyIORef (roundRef c) (+1)
               return True
      else return False
      



observeAt :: PrimEv a -> Round -> Maybe a
observeAt (PrimEv uv m) (Round ur t)
  | uv /= ur = error "Observation of TIVar from another context!"
  | otherwise = unsafePerformIO $
  do v <- readIORef m
     return $ case v of
      Just (Round _ t',a) | t' <= t -> Just a
      _                             -> Nothing

instance Eq Round where
  (Round lu lt) == (Round ru rt) | lu == ru  = lt == rt
                                 | otherwise = error "Rounds not from same clock!"

instance Ord Round where
  compare (Round lu lt) (Round ru rt)
     | lu == ru  = compare lt rt
     | otherwise = error "Rounds not from same clock!"
