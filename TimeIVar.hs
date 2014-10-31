{-# LANGUAGE TupleSections,Rank2Types #-}

module TimeIVar(TimeStamp, TimeStamper, TIVar, withTimeStamper, endRound, writeTIVar, updateTIVar, readTIVarAt, TIVarSupply, makeTIVar, runTIVarSupply) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar 
import System.IO.Unsafe
import Data.IORef




newtype TimeStamp s = TimeStamp Integer deriving (Eq,Ord)
newtype TimeStamper s   = TimeStamper (MVar Integer)
newtype TIVar       s a = TIVar (IORef (Maybe (Integer,a)))

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

writeTIVar :: TimeStamper s -> TIVar s a -> a -> IO ()
writeTIVar (TimeStamper l) (TIVar r) a = 
  do t <- takeMVar l
     v <- readIORef r
     case v of
       Just _ -> error "Written to TIVar twice!"
       _      -> return ()
     writeIORef r (Just (t,a))
     putMVar l t 

updateTIVar :: TimeStamper s -> TIVar s a -> Maybe a -> IO ()
updateTIVar (TimeStamper l) (TIVar r) a = 
  do t <- takeMVar l
     v <- readIORef r
     case v of
       Just _ -> error "Written to TIVar twice!"
       _      -> return ()
     writeIORef r (fmap (t,) a)
     putMVar l t 

readTIVarAtIO :: TimeStamp s -> TIVar s a -> IO (Maybe a)
readTIVarAtIO (TimeStamp t) (TIVar r) = 
  do v <- readIORef r
     case v of
       Just (t',a) | t' <= t -> return (Just a)
       _                     -> return Nothing

readTIVarAt ::  TIVar s a -> TimeStamp s -> Maybe a
readTIVarAt s t = unsafePerformIO $ readTIVarAtIO t s
     
newtype TIVarSupply s a = TS {fromTS :: IO a }


instance Monad (TIVarSupply s) where
  return = TS . return
  (TS x) >>= f = TS $ unsafeInterleaveIO x >>= fromTS . f

makeTIVar :: TIVarSupply s (TIVar s a)
makeTIVar = TS $ liftM TIVar (newIORef Nothing)

runTIVarSupply :: TIVarSupply s a -> IO a
runTIVarSupply (TS m) = m








instance Functor (TIVarSupply s) where
  fmap f = liftM f

instance Applicative (TIVarSupply s) where
  pure = return
  f <*> x = do fv <- f; xv <- x; return (fv xv)


