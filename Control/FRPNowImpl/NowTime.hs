
{-# LANGUAGE Rank2Types,GeneralizedNewtypeDeriving, LambdaCase #-}
module Control.FRPNowImpl.NowTime(TimeL, NowL, Time,Now, bigBang, maxTime, syncIO, startIO, everyRound, hasPassed, startNowL, startNow) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Trans.State.Strict
import Control.FRPNowImpl.ConcFlag
import Control.Applicative
import Control.Monad.Trans
import System.IO.Unsafe
import Control.Monad

data TimeStamp = MinBound | TimeStamp Integer deriving (Ord,Eq)

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
  everyRounds :: [EveryRound s],
  nextRound :: MVar Integer,
  flag :: Flag
 }

maxTime :: TimeL s -> TimeL s -> TimeL s
maxTime l r = Syn $ unsafePerformIO $ newMVar (TimeSyntax MinBound (Max l r))

bigBang :: TimeL s
bigBang = Syn $ unsafePerformIO $ newMVar (Done MinBound)

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


addER :: EveryRound s -> NowL s ()
addER r = NowL $ modify (\x -> x { everyRounds = r : everyRounds x} )

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
     tr <- updateTime r
     return $ case (tl,tr) of
       (Just x       , Just y ) -> Right (max x y)
       (Just x, Nothing)        -> Left (SameAs r)
       (Nothing, Just y)        -> Left (SameAs l)
       _                        -> Left (Max l r)

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
newEnv = Env 0 [] <$> newMVar 1 <*> newFlag
         
runRound :: NowL s ()                                 
runRound =  
  do s <- NowL $ get
     let er = everyRounds s
     NowL $ put (s {everyRounds = []})
     b <- syncIO $ takeMVar (nextRound s) 
     syncIO $ putMVar (nextRound s) (b + 1)
     mapM_ takeER er
     mapM_ runER er

runER :: EveryRound s -> NowL s ()
runER r@(EveryRound m (BaseTime mv)) =
   m >>= \case
    True -> NowL $ 
       do s <- get
          lift $ putMVar mv $ Just (curRound s)
          return ()
    False -> syncIO (putMVar mv Nothing)  >> addER r

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
 do waitForSignal flag
    e <- takeMVar n
    e' <- execStateT (runNowL runRound) e
    putMVar n e' 


startNow :: NowL Global (TimeL Global) -> IO ()
startNow m = 
  do end <- newEmptyMVar 
     e <- takeMVar globalNowL
     e' <- execStateT (runNowL $ m >>= checkEveryRound end) e
     putMVar globalNowL e'
     readMVar end where

 checkEveryRound m e = everyRound $ 
   hasPassed e >>= \case
     True  -> syncIO (putMVar m ()) >> return True
     False -> return False
                 
                

