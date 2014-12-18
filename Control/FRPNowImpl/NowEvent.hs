{-# LANGUAGE TypeSynonymInstances,Rank2Types,TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.NowEvent(Event,never,evNow,Now,runNow,runNowGlobal,syncIO,asyncIO,firstObs,planIO,Global) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef 
import Control.Monad
import System.IO.Unsafe
import Control.FRPNowImpl.ConcFlag
import Control.Monad.Reader
import Debug.Trace
import Data.Maybe

data Time = MinBound | Time Integer deriving (Ord,Eq)


newtype Event s a = E { runEv :: Time -> Maybe (Time, a) }


never = E $ const $ Nothing

instance Functor (Event s) where
  fmap f e = E $ \t -> case runEv e t of
                        Just (t,x) -> Just (t,f x)
                        Nothing  ->  Nothing


instance Applicative (Event s) where
  pure = return
  (<*>) = ap

instance Monad (Event s) where
  return x = E $ const (Just (MinBound,x))
  m >>= f  = E $ \t -> 
      -- maybe monad
      do (ta,a) <- runEv m t 
         (tx,x) <- runEv (f a) t
         Just (max ta tx, x)

first :: Event s a -> Event s a -> Event s a
first l r = E $ \t -> 
  case runEv r t of
    Just (tr,r) -> 
       Just $ case runEv (prev l) t of
         Just (tl,l) -> if tr <= tl then (tr,r) else (tl,l)
         Nothing     -> (tr,r)
    Nothing -> runEv l t

 where prev x = E $ \t -> 
        case t of
         MinBound -> Nothing
         Time i -> runEv x (Time (i - 1)) 

{-
memo :: Event s a -> Event s a
memo e = E $ \t -> unsafePerformIO $ runMemo t where
  mvar = unsafePerformIO $ newMVar (Left (Nothing,MinBound,e))
  runMemo t = 
    do r <- takeMVar mvar 
       let r' = update t r
       putMVar mvar r'
       return (getRes t r')
  update t (Left (v,d,e)) =     
           case v of
             Just tp | tp >= t -> Left (v,d,e)
             _       -> case runEv e t of
                           Left (Just (Delay d' e')) -> Left (Just t, max d d' ,e')
                           Left _  -> Left (Just t, d ,e)
                           Right v -> Right v
  update t v = v 
  getRes t (Right (tu,x)) | tu <= t = Right (tu,x)
  getRes t (Left     = Nothing

{-# NOINLINE memo #-}  
-}
{-
never = E $ const Nothing

instance Functor (Event s) where
  fmap f e = E $ \t -> case runEv e t of
                        Just (t,x) -> Just (t,f x)
                        Nothing    -> Nothing


instance Applicative (Event s) where
  pure = return
  (<*>) = ap

instance Monad (Event s) where
  return x = E $ const (Just (MinBound,x))
  m >>= f  = E $ \t -> 
      do (ta,a) <- runEv m t
         Left (Just (f a))

first :: Event s a -> Event s a -> Event s a
first l r = E $ \t -> 
  case runEv r t of
    Just (tr,r) -> 
       Just $ case runEv (prev l') t of
         Just (tl,l) -> if tr <= tl then (tr,r) else (tl,l)
         Nothing     -> (tr,r)
    Nothing -> runEv l t
 where l' = memo l 
       prev x = E $ \t -> 
        case t of
         MinBound -> Nothing
         Time i -> runEv x (Time (i - 1)) 
-}



-- end events, start now

data Env s = Env {
  curRound  :: Integer,
  nextRound :: MVar Integer,
  plans     :: MVar [Plan s],
  flag      :: Flag
 }

newtype Now s a = Now { runNow' ::  ReaderT (Env s) IO a } deriving (Functor, Applicative, Monad)

getEnv = Now $ ask

getRound :: Now s Integer
getRound =  curRound <$> getEnv 

syncIO m = Now $ lift m

data Plan s = forall a. Plan (Event s (Now s a))  (MVar Time) (MVar (Maybe (Time,a)))

type PrimEv a = MVar (Maybe (Time, a))

firstObs :: Event s a -> Event s a -> Now s (Event s a)
firstObs l r = evNow r >>= \case
   Just n  -> return (pure n)
   Nothing -> evNow l >>= \case
               Just n  -> return (pure n)
               Nothing -> return (first l r)

toEv :: PrimEv a -> Event s a 
toEv m = E $ \t -> case unsafePerformIO $ readMVar m of
            Just (tr,a) | tr <= t -> Just (tr,a)
            _ -> Nothing
{-# NOINLINE toEv #-}

asyncIO :: IO a -> Now s (Event s a)
asyncIO m = 
      do r <- syncIO $ newMVar Nothing
         env <- getEnv
         syncIO $ forkIO $ m >>= setVal env r
         return (toEv r)
 where setVal env r a = 
        do i <- takeMVar (nextRound env)
           swapMVar r (Just (Time i,a))
           signal (flag env)
           putMVar (nextRound env) i

prevRound = Time . (\x -> x - 1) <$> getRound

planIO :: Event s (Now s a) -> Now s (Event s a)
planIO e = 
  do r <- syncIO $ newEmptyMVar
     i <- prevRound
     bound <- syncIO $ newMVar i
     tryPlan (Plan e bound r)
     return (toEvPlan e bound r)



toEvPlan :: Event s (Now s a) -> MVar Time -> MVar (Maybe (Time,a)) -> Event s a 
toEvPlan e b m = E $ \t -> 
     case runEv e t of
       Just _ -> case unsafePerformIO $ readMVar m of
                    Just (tr,a) | tr <= t -> Just (tr,a)
                    _ -> Nothing
       Nothing -> Nothing
{-# NOINLINE toEvPlan #-}

evNow :: Event s a -> Now s (Maybe a)
evNow e = fmap snd <$> evNowTime e

evNowTime :: Event s a -> Now s (Maybe (Time,a))
evNowTime e = runEv e . Time <$> getRound
  
tryPlan p@(Plan e b m) = 
  evNowTime e >>= \case
   Just (_,n)  -> do i <- getRound
                     a <- n
                     syncIO $ putMVar m (Just (Time i,a))
   Nothing     -> do planPlan p
                     syncIO $ putMVar m Nothing

planPlan p =  do pl <- plans <$> getEnv
                 ls <- syncIO $ takeMVar pl
                 syncIO $ putMVar pl (p : ls)

incRound :: Env s -> IO (Env s)
incRound env = 
  do i <- takeMVar (nextRound env)
     putMVar  (nextRound env) (i + 1)
     return $ env {curRound = i}

runRound :: Env s -> IO ()
runRound env = 
      do pl <- takeMVar (plans env)
         putMVar (plans env) []
         mapM_ (lockPlan (curRound env)) pl
         --putStrLn (show $ length pl)
         mapM_ (runPlan env) pl
         mapM_ waitPlan pl
  where lockPlan i (Plan _ b m)  = swapMVar b (Time (i - 1)) >> takeMVar m >> return ()
        runPlan env p       = forkIO (runReaderT (runNow' (tryPlan p)) env)
        waitPlan (Plan _ _ m) = readMVar m >> return ()

newEnv :: IO (Env s)
newEnv = Env 0 <$> newMVar 1 <*> newMVar [] <*> newFlag

runNow :: (forall s. Now s (Event s a)) -> IO a
runNow (Now p) = 
  do env <- newEnv
     e <- runReaderT p env
     mainLoop e env where
 mainLoop e = loop where
  loop env = 
   do waitForSignal (flag env)
      env' <- incRound env
      runRound env'
      v <- runReaderT (runNow' $ evNow e) env
      case v of
       Just x -> return x
       Nothing -> loop env'
         


----- Global stuff  
        
data Global 

globalEnv :: MVar (Env Global)
globalEnv = unsafePerformIO $ 
  do e <- newEnv
     m <- newMVar e
     forkIO $ globalLoop m (flag e)
     return m
{-# NOINLINE globalEnv #-}

globalLoop :: MVar (Env Global) -> Flag -> IO ()
globalLoop envm flag = forever $  
  do waitForSignal flag
     env <- takeMVar envm
     env' <- incRound env
     runRound env'
     putMVar envm env'

runNowGlobal :: Now Global (Event Global a) -> IO a
runNowGlobal n =
  do env <- takeMVar globalEnv
     w <- newEmptyMVar 
     runReaderT (runNow' $ planWait w) env
     putMVar globalEnv env
     takeMVar w where
  planWait w = 
    do e <- n
       planIO (setMVar w <$> e)
  setMVar w a = syncIO $ putMVar w a


