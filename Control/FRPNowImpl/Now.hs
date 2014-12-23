{-# LANGUAGE ScopedTypeVariables,TypeSynonymInstances,Rank2Types,TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.Now(Now, syncIO, asyncIO, evNow, firstObs, planIO, planIOWeak, planIOWeakKey, runFRPLocal, runFRP, Global) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Debug.Trace
import Data.Maybe
import Control.FRPNowImpl.Event
import Data.TIVar
import Data.IVar
import Control.Concurrent
import Control.Concurrent.ConcFlag
import System.IO.Unsafe
import Data.Ref

-- New, simpler implementation of Now...

data Plan s = forall a. Plan (Event s (Now s a))  (IVar a)

data Env s = Env {
  clock :: Clock s,
  plans :: MVar [Ref (Plan s)],
  flag  :: Flag 
 }

newtype Now s a = Now { runNow' ::  ReaderT (Env s) IO a } deriving (Functor, Applicative, Monad)

getEnv = Now ask

syncIO :: IO a -> Now s a
syncIO m = Now $ liftIO m

asyncIO :: IO a -> Now s (Event s a)
asyncIO m =  
  do env <- getEnv
     ti <- syncIO $ newTIVar (clock env)
     syncIO $ forkIO $ m >>= writeTIVar ti >> signal (flag env)
     return $ makeEvent (observeAt ti)


evNow :: Event s a -> Now s (Maybe a)
evNow e = 
   do env <- getEnv
      t <- syncIO $ Time <$> curRound (clock env)
      return (fmap snd $ runEv e t)

firstObs :: Event s a -> Event s a -> Now s (Event s a)
firstObs l r = 
  do lv <- evNow l
     rv <- evNow r 
     return $ case (lv,rv) of
       (_, Just r) -> pure r
       (Just l, _) -> pure l
       (_, _     ) -> first l r

planIO :: Event s (Now s a) -> Now s (Event s a)
planIO  = planIO' (\_ y -> makeStrongRef y)

planIOWeak :: Event s (Now s a) -> Now s (Event s a)
planIOWeak  = planIO' makeWeakRefKey

planIOWeakKey :: k -> Event s (Now s a) -> Now s (Event s a)
planIOWeakKey k =  planIO' (\_ y -> makeWeakRefKey k y)

planIO' makeRef e = evNow e >>= \case
            Just n  -> pure <$> n
            Nothing -> do iv <- syncIO $ newIVar
                          let evRes = ivarVal iv <$ e
                          p <- syncIO $ makeRef iv (Plan e iv)
                          addPlan p
                          return evRes


addPlan :: Ref (Plan s) -> Now s ()
addPlan p = 
 do env <- getEnv
    pl <- syncIO $ takeMVar (plans env)
    syncIO $ putMVar (plans env) (p : pl)


tryPlan :: Ref (Plan s) -> Now s ()
tryPlan p = 
    syncIO (deRef p) >>= \case
       Just (Plan e iv) -> evNow e >>= \case
            Just n  -> n >>= syncIO . writeIVar iv
            Nothing -> addPlan p
       Nothing -> return ()

tryPlans :: Now s ()
tryPlans = 
  do env <- getEnv
     pl <- syncIO $ swapMVar (plans env) []
     mvars <- syncIO $ sequence (replicate (length pl) newEmptyMVar)
     mapM_ parTryPlan (zip pl mvars)
     syncIO $ mapM_ takeMVar mvars
  where parTryPlan (p,mv) = 
         do env <- getEnv
            syncIO $ forkIO $ runNow env (tryPlan p)  >> putMVar mv ()

runFRPLocal :: (forall s. Now s (Event s a)) -> IO a
runFRPLocal m = withClock $ \c -> 
     do plans <- newMVar []
        flag <- newFlag
        let env = Env c plans flag
        v <- runNow env m
        loop env v where
   loop env v = 
    runNow env (evNow v) >>= \case
         Just a -> return a
         Nothing -> 
           do waitForSignal (flag env)
              endRound (clock env) 
              runNow env tryPlans
              loop env v

runNow env m = runReaderT (runNow' m) env

-- Global stuff


data Global 

data FRPInit = forall a. FRPInit (Now Global (Event Global a)) (MVar a)

runInits :: MVar [FRPInit] -> Now Global ()
runInits inits = 
  do is <- syncIO $ swapMVar inits []
     mapM_ runInit is
  where runInit (FRPInit n m) = 
          do e <- n
             let setEm a = syncIO (putMVar m a)
             planIO (setEm <$> e)

globalLoop :: Env Global -> MVar [FRPInit] -> IO ()
globalLoop env init = forever $ 
   do waitForSignal (flag env)
      endRound (clock env)  
      runNow env tryPlans
      runNow env (runInits init)


global :: (Flag, MVar [FRPInit])
global = unsafePerformIO $ unsafeWithClock $ \c ->
    do flag <- newFlag
       init <- newMVar []
       plans <- newMVar []
       let env = Env c plans flag
       forkIO $ globalLoop env init
       return (flag, init)
{-# NOINLINE global #-}  

runFRP :: Now Global (Event Global a) -> IO a
runFRP n = do m <- newEmptyMVar 
              let (flag,inits) = global
              is <- takeMVar inits
              putMVar inits (FRPInit n m : is)
              signal flag
              takeMVar m
              

