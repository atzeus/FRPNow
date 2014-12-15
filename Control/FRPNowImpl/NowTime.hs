
{-# LANGUAGE TupleSections,Rank2Types,GeneralizedNewtypeDeriving, LambdaCase #-}
module Control.FRPNowImpl.NowTime(TimeL, NowL, Time,Now, bigBang, pigsFly, maxTime, syncIO, startIO, doAtTime, doFutureTime,hasPassed, earliestObs, startNowL, startNowTime) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Trans.State.Strict
import Control.FRPNowImpl.ConcFlag
import Control.Applicative
import Control.Monad.Trans
import System.IO.Unsafe
import Control.Monad
import Debug.Trace

{-
Monad Event
never
asyncIO :: IO a -> Now (Event a)
plan    :: Event (Now a) -> Now (Event a)
firstObs :: Event a -> Event a -> Now (Event a)
-}


data TimeStamp = MinBound | TimeStamp Integer | MaxBound deriving (Ord,Eq)

data TimeSyntax s 
  = Max (TimeL s) (TimeL s)
  | SameAs (TimeL s)

data TimeState s = Done TimeStamp
                 | TimeSyntax { lastUpdate :: TimeStamp, syntax :: TimeSyntax s} 

data TimeL s
  = BaseTime (MVar (Maybe Integer))
  | Syn (MVar (TimeState s))

data EveryRound s = EveryRound {
  instr  :: NowL s Bool,
  result :: TimeL s
 }

data Env s = Env {
  curRound :: Integer,
  everyRounds :: MVar [EveryRound s],
  nextRound :: MVar Integer,
  flag :: Flag
 }

maxTime :: TimeL s -> TimeL s -> TimeL s
maxTime l r = Syn $ unsafePerformIO $ newMVar (TimeSyntax MinBound (Max l r))

bigBang :: TimeL s
bigBang = Syn $ unsafePerformIO $ newMVar (Done MinBound)

pigsFly :: TimeL s
pigsFly = BaseTime $ unsafePerformIO $ newMVar Nothing 

newtype NowL s a = NowL { runNowL :: StateT (Env s) IO a } deriving (Monad, Functor, Applicative)

syncIO :: IO a -> NowL s a
syncIO m = NowL (lift m)

startIO :: IO () -> NowL s (TimeL s)
startIO m = NowL $ 
      do s <- get
         r <- lift $ newMVar Nothing
         lift $ forkIO $ m >>= setVal (flag s) (nextRound s) r
         return (BaseTime r)
 where setVal flag nextRound r a = 
        do i <- takeMVar nextRound
           swapMVar r (Just i)
           putMVar nextRound i
           signal flag

doAtTime :: TimeL s -> NowL s () -> NowL s ()
doAtTime t n = doFutureTime t (n >> return bigBang) >> return ()

doFutureTime :: TimeL s -> NowL s (TimeL s) -> NowL s (TimeL s)
doFutureTime t n = 
  do m <- syncIO $ newMVar (Left t) 
     everyRound $
       do v <- syncIO $ takeMVar m 
          (v',end) <- case v of
            Left t -> hasPassed t >>= \case
                        False -> return (v,False)
                        True  -> do t2 <- n
                                    done <- hasPassed t2
                                    return (Right t2, done)
            Right t -> (v,) <$> hasPassed t
          syncIO $ putMVar m v'
          return end
                 

addER :: EveryRound s -> NowL s ()
addER r = NowL $ do s <- everyRounds <$> get
                    v <- lift $ takeMVar s
                    lift $ putMVar s (r : v)


everyRound :: NowL s Bool -> NowL s (TimeL s)
everyRound m = do mv <- syncIO newEmptyMVar 
                  let ev = BaseTime mv
                  runER (EveryRound m ev)
                  return ev

trySyntax :: TimeSyntax s -> NowL s (Either (TimeSyntax s) TimeStamp)
trySyntax (SameAs t) = 
  do tl <- updateTime t
     return $ case tl of
       Just x   -> Right x
       Nothing  -> Left (SameAs t)
trySyntax (Max l r) = 
  do tl <- updateTime l
     case tl of
       Just x -> updateTime r >>= \case
                   Just y -> return (Right (min x y))
                   Nothing -> return (Left (SameAs r))
       Nothing -> return $ Left (Max l r)

updateTime :: TimeL s -> NowL s (Maybe TimeStamp)
updateTime (BaseTime m) = NowL $ lift $ fmap TimeStamp <$> readMVar m
updateTime (Syn m)      =
  do r <- getCurRound
     s <- syncIO $ takeMVar m
     s' <- case s of
      Done i -> return s
      TimeSyntax lu sn ->
         if lu == r
         then return s
         else trySyntax sn >>= \case
               Right i -> return (Done i)
               Left sn' -> return (TimeSyntax r sn')
     syncIO $ putMVar m s'
     return $ case s' of
        Done i | i >= r -> Just i
        _      -> Nothing

hasPassed :: TimeL s -> NowL s Bool
hasPassed t =  
  do v <- updateTime t
     r <- getCurRound
     return $ case v of
      Just b  -> r >= b 
      Nothing -> False

earliestObs :: TimeL s -> TimeL s -> NowL s (TimeL s)
earliestObs l r = everyRound $ (||) <$> hasPassed l <*> hasPassed r

getCurRound = NowL $ TimeStamp . curRound <$> get

startNowL :: (forall s. NowL s (TimeL s)) -> IO ()
startNowL m = do env <- newEnv
                 evalStateT (runNowL $ m >>= loop (flag env)) env where
  loop :: Flag -> TimeL s -> NowL s ()
  loop flag ev = 
    do syncIO $ waitForSignal flag
       runRound 
       e <- hasPassed ev
       if e then return () else loop flag ev

newEnv :: IO (Env s)
newEnv = Env 0 <$> newMVar [] <*> newMVar 1 <*> newFlag
         
runRound :: NowL s ()                                 
runRound =  
  do s <- NowL $ get
     let er = everyRounds s
     trace "starting round" $ return ()
     er <- syncIO $ takeMVar (everyRounds s)
     syncIO $ putMVar (everyRounds s) []
     b <- syncIO $ takeMVar (nextRound s) 
     syncIO $ putMVar (nextRound s) (b + 1)
     mapM_ takeER er
     trace "gonna run!" $ return ()
     syncIO $ mapM_ (runERFork s) er
     mapM_ readER er

runERFork s v = forkIO $ evalStateT (runNowL $ runER v) s

runER :: EveryRound s -> NowL s ()
runER r@(EveryRound m (BaseTime mv)) =
   m >>= \case
    True -> NowL $ 
       do s <- get
          trace "RunER" $ return ()
          lift $ putMVar mv $ Just (curRound s)
          return ()
    False -> trace "Fail!" $ syncIO (putMVar mv Nothing)  >> addER r
readER (EveryRound _ (BaseTime mv)) = syncIO (readMVar mv) >> return ()
takeER (EveryRound _ (BaseTime mv)) = syncIO (takeMVar mv) >> return ()

-- global NowL loop 

data Global

type Now = NowL Global
type Time = TimeL Global

globalNowL :: MVar (Env Global)
globalNowL = unsafePerformIO $ 
      do e <- newEnv
         n <- newMVar e
         forkIO $ globalLoop n (flag e)
         return n
{-# NOINLINE globalNowL #-}

globalLoop :: MVar (Env Global) -> Flag -> IO ()
globalLoop n flag  = forever $ 
 do 
    waitForSignal flag
    trace "got flag" $ return ()
    e <- takeMVar n
    trace "bla" $ return ()
    e' <- execStateT (runNowL runRound) e
    trace "end round" $ return ()
    putMVar n e' 


startNowTime :: NowL Global (TimeL Global) -> IO ()
startNowTime m = 
  do end <- newEmptyMVar 
     e <- takeMVar globalNowL
     trace "start Now!" $ return ()
     e' <- execStateT (runNowL $ m >>= checkEveryRound end) e
     trace "End now!" $ return ()
     putMVar globalNowL e'
     readMVar end where

 checkEveryRound m e = everyRound $ 
   trace "bla" $ hasPassed e >>= \case
     True  -> syncIO (putMVar m ()) >> return True
     False -> return False
                 
                

