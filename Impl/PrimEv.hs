{-# LANGUAGE LambdaCase  #-}
module Impl.PrimEv(Round, Clock, PrimEv, newClock , spawn, curRound, waitEndRound ,observeAt,getCallback ) where

import Impl.ConcFlag
import Control.Concurrent.MVar
import Control.Applicative
import System.IO.Unsafe
import Data.Unique
import Control.Concurrent

data Clock    = Clock Unique Flag (MVar Integer)
data Round    = Round Unique Integer
data PrimEv a = PrimEv Unique (MVar (Maybe (Round, a)))

newClock :: IO Clock
newClock = Clock <$> newUnique <*> newFlag <*> newMVar 0

-- give an event that occurs when the also
-- returned function is called
-- useful for interfacing with callback-based
-- systems
getCallback :: Clock -> IO (PrimEv a, a -> IO ())
getCallback (Clock u flag round) =
  do mv <- newMVar Nothing
     return (PrimEv u mv, setValue mv)
  where setValue mv x =
         do i <- takeMVar round
            let i' = Round u (i + 1)
            swapMVar mv (Just (i', x))
            putMVar round i
            signal flag


spawn :: Clock -> IO a ->  IO (PrimEv a)
spawn c m =
  do (pe, call) <- getCallback c
     forkIO $ m >>= call
     return pe


curRound :: Clock -> IO Round
curRound (Clock u _ c) = Round u <$> readMVar c

waitEndRound :: Clock -> IO ()
waitEndRound (Clock u f c) =
   do waitForSignal f
      i <- takeMVar c
      putMVar c (i+1)


observeAt :: PrimEv a -> Round -> Maybe a
observeAt (PrimEv uv m) (Round ur t)
  | uv /= ur = error "Observation of TIVar from another context!"
  | otherwise = unsafePerformIO $
  do v <- readMVar m
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
