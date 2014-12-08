
{-# LANGUAGE Rank2Types,GeneralizedNewtypeDeriving,ExistentialQuantification,GADTs,ScopedTypeVariables, LambdaCase #-}
module Control.FRPNowImpl.Now(Event, Now, syncIO, asyncIO, everyRound, getEv, startNowGlobal) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Trans.State.Strict
import Control.FRPNowImpl.ConcFlag
import Control.Applicative
import Control.Monad.Trans
import System.IO.Unsafe
import Control.Monad

data Event s a 
  = Event (MVar (Maybe (Integer, a)))

data EveryRound s = forall a. EveryRound {
  instr  :: Now s (Maybe a),
  result :: Event s a
 }

data Env s = Env {
  curRound :: Integer,
  everyRounds :: [EveryRound s],
  nextRound :: MVar Integer,
  flag :: Flag
 }

newtype Now s a = Now { runNow :: StateT (Env s) IO a } deriving (Monad, Functor, Applicative)

syncIO :: IO a -> Now s a
syncIO m = Now (lift m)

asyncIO :: IO a -> Now s (Event s a)
asyncIO m = Now $ 
      do s <- get
         r <- lift $ newMVar Nothing
         lift $ forkIO $ m >>= setVal (flag s) (nextRound s) r
         return (Event r)
 where setVal flag nextRound r a = 
        do i <- takeMVar nextRound
           swapMVar r (Just (i,a))
           putMVar nextRound i
           signal flag

runER :: EveryRound s -> Now s ()
runER r@(EveryRound m (Event mv)) =
   m >>= \case
    Just x  -> Now $ do s <- get; lift (putMVar mv (Just (curRound s,x))) >> return ()
    Nothing -> syncIO (putMVar mv Nothing)  >> addER r

takeER (EveryRound _ (Event mv)) = syncIO (takeMVar mv) >> return ()

addER :: EveryRound s -> Now s ()
addER r = Now $ modify (\x -> x { everyRounds = r : everyRounds x} )

everyRound :: Now s (Maybe a) -> Now s (Event s a)
everyRound m = do mv <- syncIO newEmptyMVar 
                  let ev = Event mv
                  runER (EveryRound m ev)
                  return ev

getEv :: Event s a -> Now s (Maybe a)
getEv (Event m) = Now $
  do v <- lift $ readMVar m
     case v of
      Just (b,a) -> 
       do r <- curRound <$> get
          return $ if r >= b then Just a else Nothing
      Nothing -> return Nothing

startNow :: (forall s. Now s (Event s a)) -> IO a
startNow m = do env <- newEnv
                evalStateT (runNow $ m >>= loop) env where
  loop :: Event s a -> Now s a
  loop ev = 
    do runRound 
       e <- getEv ev
       case e of
        Just x -> return x
        Nothing -> loop ev

newEnv :: IO (Env s)
newEnv = Env 0 [] <$> newMVar 1 <*> newFlag
         
runRound :: Now s ()                                 
runRound =  
  do s <- Now $ get
     syncIO $ waitForSignal (flag s)
     let er = everyRounds s
     Now $ put (s {everyRounds = []})
     b <- syncIO $ takeMVar (nextRound s) 
     syncIO $ putMVar (nextRound s) (b + 1)
     mapM_ takeER er
     mapM_ runER er


-- global now loop 

data Global

globalNow :: MVar (Env Global)
globalNow = unsafePerformIO $ 
      do e <- newEnv
         n <- newMVar e
         forkIO $ globalLoop n (flag e)
         return n
{-# NOINLINE globalNow #-}

globalLoop :: MVar (Env Global) -> Flag -> IO ()
globalLoop n flag  = forever $ 
 do waitForSignal flag
    e <- takeMVar n
    e' <- execStateT (runNow runRound) e
    putMVar n e' 


startNowGlobal :: Now Global (Event Global a) -> IO a
startNowGlobal m = 
  do end <- newEmptyMVar 
     e <- takeMVar globalNow
     e' <- execStateT (runNow $ m >>= checkEveryRound end) e
     putMVar globalNow e'
     readMVar end where

 checkEveryRound m e = everyRound $ 
   getEv e >>= \case
     Just x -> syncIO (putMVar m x) >> return (Just ())
     Nothing -> return Nothing
                 
                

