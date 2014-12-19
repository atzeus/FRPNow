{-# LANGUAGE Rank2Types,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.ASync(Timestamp, RoundM, ASync,TimeIVar, prevTimestamp, async, waitEndRound, prevRound, observeAt, forkASync, runASync,runRoundM,unsafeRunRoundM) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.FRPNowImpl.ConcFlag

data Timestamp s = Timestamp Integer deriving (Ord,Eq)

newtype RoundM s a = RoundM (ReaderT (Flag,MVar Integer) IO a) deriving (Monad,Applicative,Functor)
newtype ASync s a = ASync (ReaderT (Flag,MVar Integer) IO a) deriving (Monad,Applicative,Functor)


newtype TimeIVar s a = TimeIVar (MVar (Maybe (Timestamp s, a)))


prevTimestamp :: Timestamp s -> Timestamp s
prevTimestamp (Timestamp i) = Timestamp (i - 1)

instance MonadIO (ASync s) where
  liftIO m = ASync $ lift m

waitEndRound :: RoundM s ()
waitEndRound = RoundM $ 
     do (flag,curRound) <- ask
        liftIO $ waitForSignal flag
        i <- liftIO $ takeMVar curRound
        liftIO $ putMVar curRound (i + 1)

runASync :: ASync s a -> RoundM s a
runASync (ASync m) = RoundM m

async :: IO a -> ASync s (TimeIVar s a)
async m = ASync $ 
     do r <- liftIO $ newMVar Nothing
        (flag,curRound) <- ask
        liftIO $ forkIO $ m >>= setVal flag curRound r
        return (TimeIVar r)
 where setVal flag curRound r a = 
        do i <- takeMVar curRound
           swapMVar r (Just (Timestamp i,a))
           putMVar curRound i
           signal flag

forkASync :: ASync s () -> ASync s ()
forkASync (ASync m) = ASync $ 
  do (flag,round) <- ask
     liftIO $ forkIO $ runReaderT m (flag,round)
     return ()

prevRound :: ASync s (Timestamp s)
prevRound =  ASync $ 
     do (_,curRound) <- ask
        i <- liftIO $ readMVar curRound
        return (Timestamp (i - 1))

observeAt :: TimeIVar s a -> Timestamp s -> Maybe (Timestamp s, a)
observeAt (TimeIVar m) t = 
  case unsafePerformIO $ readMVar m of
      Just (t',a) | t' <= t -> Just (t',a)
      _               -> Nothing

runRoundM :: (forall s. RoundM s a) -> IO a
runRoundM (RoundM m) = 
  do round <- newMVar 0
     flag <- newFlag
     runReaderT m (flag,round)
  
unsafeRunRoundM :: Flag -> RoundM s a -> IO a
unsafeRunRoundM flag (RoundM m) = 
  do round <- newMVar 0
     runReaderT m (flag,round)



