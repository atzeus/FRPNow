{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, Rank2Types,RecursiveDo #-}

module TimeIVar(TimeStamp, TimeStamper, TIVar, withTimeStamper, endRound, newTIVar, writeTIVar,tvarAt,TIVarSupply,makeTIVar,runTIVarSupply ) where

import Control.Applicative hiding (empty)
import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar 
import System.IO.Unsafe
import Data.Maybe


newtype TimeStamp   s   = TimeStamp Integer deriving (Eq,Ord)
newtype TimeStamper s   = TimeStamper (MVar Integer)
newtype TIVar       s a = TIVar (MVar (Integer ,a))



withTimeStamper :: (forall s. TimeStamper s -> IO a) -> IO a
withTimeStamper f = 
  do l <- newMVar 0
     f (TimeStamper l)

-- ends the current round, and gives back the 
-- the number of the round that just ended
-- Afterwards we can be sure that no more 
-- IVars get timestamp <= t 
endRound :: TimeStamper s -> IO (TimeStamp s)
endRound (TimeStamper l) = 
    do t <- takeMVar l
       putMVar l (t + 1)
       return (TimeStamp t)

newTIVar :: IO (TIVar s a)
newTIVar = TIVar <$> newEmptyMVar

writeTIVar :: TimeStamper s -> TIVar s a -> a -> IO ()
writeTIVar (TimeStamper l) (TIVar r) a = 
  do t <- takeMVar l
     v <- tryTakeMVar r
     maybe (return ()) (error "Written to TIVar twice!")
     putMVar r (Just (t,a))
     putMVar l t 

tvarAt ::  TIVar s a -> TimeStamp s -> Maybe a
tvarAt s t = unsafePerformIO $ readTIVarAtIO t s


readTIVarAtIO :: TimeStamp s -> TIVar s a -> IO (Maybe a)
readTIVarAtIO (TimeStamp t) (TIVar r) = 
  do v <- tryReadMVar r
     case v of
       Just  (t',a) | t' <= t -> return (Just a)
       _                      -> return Nothing
