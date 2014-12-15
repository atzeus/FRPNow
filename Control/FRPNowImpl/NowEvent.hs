{-# LANGUAGE TypeSynonymInstances,Rank2Types,TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.NowEvent(Event,never,evNow,Now,runNow,runNowGlobal,syncIO,asyncIO,firstObsNow,planIO,Global) where

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

instance Functor (Event s) where
  fmap = liftM

instance Applicative (Event s) where
  pure = return
  (<*>) = ap

data Time = MinBound | Time Integer | MaxBound deriving (Show,Ord,Eq)

instance Bounded Time where
  minBound = MinBound
  maxBound = MaxBound

evNow :: Event s a -> Now s (Maybe a)
evNow e =  evNowTime e >>= return . \case 
             Right (_,x) -> Just x
             Left _      -> Nothing

evNowTime :: Event s a -> Now s (Either Bool (Time, a))
evNowTime e = 
 do e' <- rewriteEv e 
    r <- case e' of
     Never           -> return $ Left True
     Ret x           -> return $ Right (minBound,x)
     Delay t x       -> evNowTime x >>= return . \case 
                         Left b     -> Left b
                         Right (t',e) -> Right (max t t', e)
     r               -> return $ Left False
    return r
{-
instance Show (EventSyntax s a) where
  show (Prim _) = "p"
  show Never    = "n"
  show (Delay t e) = "d" ++ show t ++ show e
  show (First l r) = "f(" ++ show l ++ "," ++ show r ++ ")"
  show (Ret x)     = "r"
  show (Bind d f)  = "b" ++ show d
-}
data EventSyntax s a 
  = Prim (PrimEv s a)
  | Never
  | Delay Time (Event s a)
  | Ret a
  | forall x. Bind (Event s x) (x -> Event s a) 

-- start non-shared events
{-
type Event = EventSyntax
instance Monad (EventSyntax s)where
  return = Ret
  (>>=)  = Bind
       
never = Never
first = First
delay = Delay
primEv = Prim . PrimEv
rewriteEv :: Event s a -> Now s (Event s a)
rewriteEv = rewriteEvSyntax
-- end non-shared events 
-}
-- start shared events

newtype Event s a = Ev (MVar (Time, EventSyntax s a))

newEvent :: EventSyntax s a -> Event s a
newEvent s = Ev $ unsafePerformIO $ newMVar (minBound,s)
{-# NOINLINE newEvent #-}

instance Monad (Event s) where
  return x = newEvent (Ret x)
  m >>= f  = newEvent (Bind m f)

never = newEvent Never
delay t = newEvent . Delay t
primEv  = newEvent . Prim . PrimEv



rewriteEv :: Event s a -> Now s (EventSyntax s a)
rewriteEv (Ev e) = 
  do (i, s) <- syncIO $ takeMVar e
     j      <- Time <$> getRound
     s' <- if i == j then return s else rewriteEvSyntax s
     syncIO $ putMVar e (j,s')
     return s'
--- end shared events 

                   
rewriteEvSyntax :: EventSyntax s a -> Now s (EventSyntax s a)
rewriteEvSyntax = \case 
  Never     -> return Never
  Prim e   -> getPrimEv e >>= return . \case 
                Just (i,x)  -> Delay i (return x)
                Nothing     -> Prim e
  Bind e f  -> evNowTime e >>= \case 
                Right  (i,x) -> rewriteEv (delay i (f x))
                Left False  -> return (Bind e f)
                Left True   -> return Never
  Delay t b -> rewriteEv b >>= return . \case
                   Never       -> Never
                   Delay t' b' -> Delay (max t t') b'
                   _           -> Delay t b
  Ret x     -> return (Ret x)

earliest (Just (i,a)) (Just (j,b)) 
             | j <= i    = Just (j,b)
             | otherwise = Just (i,a)
earliest _ (Just v)      = Just v
earliest (Just v) _      = Just v

earliest Nothing Nothing = Nothing



newtype PrimEv s a = PrimEv (MVar (Maybe (Integer, a)))

-- end events, start now

data Env s = Env {
  curRound  :: Integer,
  nextRound :: MVar Integer,
  plans     :: MVar [Plan s],
  flag      :: Flag
 }

newtype Now s a = Now { runNow' ::  ReaderT (Env s) IO a } deriving (Functor, Applicative, Monad)

getEnv = Now $ ask

getPrimEv :: PrimEv s a -> Now s (Maybe (Time,a))
getPrimEv (PrimEv r) = 
  do j <- getRound
     v <- syncIO $ readMVar r
     return $ v >>= laterThanNow j where
  laterThanNow j (i,a) = if i <= j then Just (Time i,a) else Nothing

getRound :: Now s Integer
getRound =  curRound <$> getEnv 

syncIO m = Now $ lift m

data Plan s = forall a. Plan (Event s (Now s a))  (MVar (Maybe (Integer, a)))
            | forall a. First (Event s a) (Event s a) (MVar (Maybe (Integer, a)))

asyncIO :: IO a -> Now s (Event s a)
asyncIO m = 
      do r <- syncIO $ newMVar Nothing
         env <- getEnv
         syncIO $ forkIO $ m >>= setVal env r
         return (primEv r)
 where setVal env r a = 
        do i <- takeMVar (nextRound env)
           swapMVar r (Just (i,a))
           signal (flag env)
           putMVar (nextRound env) i

planIO :: Event s (Now s a) -> Now s (Event s a)
planIO e = 
  do r <- syncIO $ newEmptyMVar
     tryPlan (Plan e r)
     return (primEv r)

firstObsNow :: Event s a -> Event s a -> Now s (Event s a)
firstObsNow l r = 
    do m <- syncIO $ newEmptyMVar
       tryPlan (First l r m)
       return (primEv m)

tryPlan :: Plan s -> Now s ()
tryPlan p@(First l r m) = 
  evNowTime r >>= \case 
    Right (_,a) -> do i <- getRound
                      syncIO $ putMVar m (Just (i,a))
    Left x -> evNowTime l >>= \case 
                Right (_, a) -> do i <- getRound
                                   syncIO $ putMVar m (Just (i,a))
                Left y -> do pl <- plans <$> getEnv
                             if x && y
                             then return ()
                             else do ls <- syncIO $ takeMVar pl
                                     syncIO $ putMVar pl (p : ls)
                             syncIO $ putMVar m Nothing
                              

tryPlan p@(Plan e m) = 
  evNowTime e >>= \case
   Right (_,n)  -> do i <- getRound
                      a <- n
                      syncIO $ putMVar m (Just (i,a))
   Left False  -> do pl <- plans <$> getEnv
                     ls <- syncIO $ takeMVar pl
                     syncIO $ putMVar pl (p : ls)
                     syncIO $ putMVar m Nothing
   Left True    -> syncIO $ putMVar m Nothing

incRound :: Env s -> IO (Env s)
incRound env = 
  do i <- takeMVar (nextRound env)
     putMVar  (nextRound env) (i + 1)
     return $ env {curRound = i}

runRound :: Env s -> IO ()
runRound env = 
      do pl <- takeMVar (plans env)
         putMVar (plans env) []
         mapM_ lockPlan pl
         putStrLn (show $ length pl)
         mapM_ (runPlan env) pl
         mapM_ waitPlan pl
  where lockPlan (Plan _ m)    = takeMVar m >> return ()
        lockPlan (First _ _ m) = takeMVar m >> return ()
        runPlan env p       = forkIO (runReaderT (runNow' (tryPlan p)) env)
        waitPlan (Plan _ m) = readMVar m >> return ()
        waitPlan (First _ _ m) = readMVar m >> return ()

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


