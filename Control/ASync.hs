{-# LANGUAGE Rank2Types,GeneralizedNewtypeDeriving #-}
module Control.ASync(Timestamp, RoundM, ASync,TimeIVar, prevTimestamp, async, waitEndRound, prevRound, observeAt, forkASync, runASync,runRoundM,unsafeRunRoundM) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.Concurrent.ConcFlag

data Timestamp s = Timestamp Integer deriving (Ord,Eq)

data Env s = Env {
  flag :: Flag,
  curRound :: MVar Integer,
  waits    :: MVar [MVar ()]
 }

newtype RoundM s a = RoundM (ReaderT (Env s) IO a) deriving (Monad,Applicative,Functor)
newtype ASync s a = ASync (ReaderT (Env s ) IO a) deriving (Monad,Applicative,Functor)

newtype TimeIVar s a = TimeIVar (MVar (Maybe (Timestamp s, a)))

prevTimestamp :: Timestamp s -> Timestamp s
prevTimestamp (Timestamp i) = Timestamp (i - 1)

instance MonadIO (ASync s) where
  liftIO m = ASync $ lift m

waitEndRound :: RoundM s ()
waitEndRound = RoundM $ 
     do env <- ask
        w <- liftIO $ takeMVar (waits env)
        liftIO $ mapM_ takeMVar w
        liftIO $ waitForSignal (flag env)
        liftIO $ putMVar (waits env) []
        i <- liftIO $ takeMVar (curRound env)
        liftIO $ putMVar (curRound env) (i + 1)
          

runASync :: ASync s a -> RoundM s a
runASync (ASync m) = RoundM m

async :: IO a -> ASync s (TimeIVar s a)
async m = ASync $ 
     do r <- liftIO $ newMVar Nothing
        env <- ask
        liftIO $ forkIO $ m >>= setVal env r
        return (TimeIVar r)
 where setVal env r a = 
        do i <- takeMVar (curRound env)
           swapMVar r (Just (Timestamp i,a))
           putMVar (curRound env) i
           signal (flag env)

forkASync :: ASync s () -> ASync s ()
forkASync (ASync m) = ASync $ 
  do env <- ask
     done <- liftIO $ newEmptyMVar
     w <- liftIO $ takeMVar (waits env)
     liftIO $ putMVar (waits env) (done : w)
     liftIO $ forkIO $ runReaderT (m >> liftIO (putMVar done ())) env
     return ()

prevRound :: ASync s (Timestamp s)
prevRound =  ASync $ 
     do env <- ask
        i <- liftIO $ readMVar (curRound env)
        return (Timestamp (i - 1))

observeAt :: TimeIVar s a -> Timestamp s -> Maybe (Timestamp s, a)
observeAt (TimeIVar m) t = 
  case unsafePerformIO $ readMVar m of
      Just (t',a) | t' <= t -> Just (t',a)
      _               -> Nothing

runRoundM :: (forall s. RoundM s a) -> IO a
runRoundM (RoundM m) = 
  do round <- newMVar 0
     waits <- newMVar []
     flag <- newFlag
     runReaderT m (Env flag round waits)
  
unsafeRunRoundM :: Flag -> RoundM s a -> IO a
unsafeRunRoundM flag (RoundM m) = 
  do round <- newMVar 0
     waits <- newMVar []
     runReaderT m (Env flag round waits)



