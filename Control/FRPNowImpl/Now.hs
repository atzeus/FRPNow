{-# LANGUAGE ScopedTypeVariables,TypeSynonymInstances,Rank2Types,TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.Now where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Debug.Trace
import Data.Maybe
import Control.FRPNowImpl.ASync
import Control.FRPNowImpl.Event
import Control.FRPNowImpl.IVar

data Plan s = forall a. Plan (Event s (Now s a))  (IVar a)

newtype Now s a = Now { runNow' ::  ReaderT (MVar [Plan s]) (ASync s) a } deriving (Functor, Applicative, Monad)

syncIO :: IO a -> Now s a
syncIO m = Now $ liftIO m

asyncIO :: IO a -> Now s (Event s a)
asyncIO m = Now $ makeEvent . observeAt <$> lift (async m)


getEv :: Event s a -> Now s (Maybe a)
getEv (E f) = fmap snd . f . Time <$> Now (lift $ curRound)

planIO :: Event s (Now s a) -> Now s (Event s a)
planIO e = getEv e >>= \case
            Just n  -> pure <$> n
            Nothing -> do iv <- syncIO $ newIVar
                          addPlan (Plan e iv)
                          return (ivarVal iv <$ e)
 
addPlan :: Plan s -> Now s ()
addPlan p = Now $
 do plmv <- ask
    pl <- liftIO $ takeMVar plmv
    liftIO $ putMVar plmv (p : pl)

tryPlan :: Plan s -> Now s ()
tryPlan (Plan e iv) = getEv e >>= \case
            Just n  -> n >>= syncIO . writeIVar iv
            Nothing -> addPlan (Plan e iv)

tryPlans :: Now s ()
tryPlans = Now $ 
  do plmv <- ask
     pl <- liftIO $ swapMVar plmv []
     mv <- liftIO $ sequence $ replicate (length pl) newEmptyMVar
     mapM_ (parTryPlan plmv ) (zip pl mv)
     liftIO $ mapM_ takeMVar mv
  where parTryPlan plmv (p,mv) = lift $ forkASync $ 
            do runReaderT (runNow' (tryPlan p)) plmv 
               liftIO $ putMVar mv ()

runFRP :: (forall s. Now s (Event s a)) -> IO a
runFRP m = runRoundM $ 
     do mv <- runASync $ liftIO $ newMVar []
        v <- runNow mv m
        loop mv v where
   loop mv v = 
    runNow mv (getEv v) >>= \case
         Just a -> return a
         Nothing -> 
           do waitEndRound 
              runNow mv tryPlans
              loop mv v
   runNow mv n = runASync (runReaderT (runNow' n) mv)  

