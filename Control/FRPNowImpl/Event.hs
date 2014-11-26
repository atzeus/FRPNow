{-# LANGUAGE TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.Event((>$<),Event,never,evNow,Now,runNow,syncIO,asyncIO,first,planIO, planFirst, Time(..),getRound,checkAll) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef 
import Control.Monad
import System.IO.Unsafe
import Control.FRPNowImpl.ConcFlag
import Debug.Trace

infixr 4 >$<

(>$<) = flip (<$>)
data Time = MinBound | Time Integer | MaxBound deriving (Show,Ord,Eq)

instance Bounded Time where
  minBound = MinBound
  maxBound = MaxBound






evNow :: Event a -> Now (Maybe (Time, a))
evNow e = rewriteEv e >>= \case
    Ret x           -> return $ Just (minBound,x)
    Delay t x       -> evNow x >$< \case 
                        Nothing     -> Nothing
                        Just (t',e) -> Just (max t t', e)
    r               -> return $ Nothing

-- start non-shared events
{-
data Event a 
  = Prim (PrimEv a)
  | Delay Time (Event a)
  | Ret a
  | forall x. Bind (Event x) (x -> Event a) 

instance Monad Event where
  return = Ret
  (>>=)  = Bind
       
delay = Delay
primEv = Prim

rewriteEv :: Event a -> Now (Event a)
rewriteEv = \case 
-- end non-shared events 
-}
-- start shared events

data EventSyntax a 
  = Prim (PrimEv a)
  | Delay Time (Event a)
  | Ret a
  | forall x. Bind (Event x) (x -> Event a) 

newtype Event a = Ev (MVar (Time, EventSyntax a))

newEvent :: EventSyntax a -> Event a
newEvent s = Ev $ unsafePerformIO $ newMVar (minBound,s)
{-# NOINLINE newEvent #-}

instance Monad Event where
  return x = newEvent (Ret x)
  m >>= f  = newEvent (Bind m f)

delay t = newEvent . Delay t
primEv  = newEvent . Prim

rewriteEv :: Event a -> Now (EventSyntax a)
rewriteEv (Ev e) = 
  do (i, s) <- syncIO $ takeMVar e
     j      <- Time <$> getRound
     s' <- if i == j then return s else rewriteEvSyntax s
     syncIO $ putMVar e (j,s')
     return s'
                   
rewriteEvSyntax :: EventSyntax a -> Now (EventSyntax a)
rewriteEvSyntax = \case 
--- end shared events 
  Prim e   -> getPrimEv e >$< \case 
                Just (i,x)  -> Delay i (return x)
                Nothing     -> Prim e
  Bind e f  -> evNow e >>= \case 
                Just  (i,x) -> rewriteEv (delay i (f x))
                Nothing     -> return (Bind e f)
  Delay t b -> rewriteEv b >$< \case
                   Delay t' b' -> Delay (max t t') b'
                   _           -> Delay t b
  Ret x     -> return (Ret x)

never = primEv Never


data PrimEv a = External (MVar (Maybe (Time, a)))
              | Internal (MVar (Maybe (Time,a)))
              | Never

newtype Now a = Now { runNow' :: IO a } deriving (Functor, Applicative, Monad)

getPrimEv :: PrimEv a -> Now (Maybe (Time,a))
getPrimEv (External r) = Now $ readMVar r >>= \case 
                                   Just (i,a) -> do j <- readMVar globalCurRound
                                                    if i <= Time j 
                                                    then return (Just (i,a))
                                                    else return Nothing
                                   Nothing    -> return Nothing
                           
getPrimEv (Internal r) = Now $ readMVar r
getPrimEv Never        = return Nothing

getRound :: Now Integer
getRound = Now $ readMVar globalCurRound

data WatchEv a = WatchFirst [Event a]

data Watch = forall a. Watch { evs :: [Event (Now a)], mvar :: MVar (Maybe (Time,a))}

runNow :: Now (Event a) -> IO a
runNow (Now p) = 
  do takeMVar globalLock
     e <- p
     m <- newEmptyMVar 
     runNow' $ planIO $ fmap (signalDone m) e
     globalFlag `seq` putMVar globalLock ()
     takeMVar m
  where signalDone :: MVar a -> a -> Now a
        signalDone m x = syncIO $ putMVar m x >> return x


asyncIO :: IO a -> Now (Event a)
asyncIO m = syncIO $ 
      do r <- newMVar Nothing
         forkIO $ m >>= setVal r
         return (primEv (External r))
 where setVal r a = 
        do i <- takeMVar globalNextRound
           takeMVar r
           putMVar r (Just (Time i,a))
           putMVar globalNextRound i
           signal globalFlag
              

syncIO :: IO a -> Now a
syncIO m = Now m

first :: [Event a] -> Now (Event a)
first e = planFirst (map (fmap pure) e)

planIO :: Event (Now a) -> Now (Event a)
planIO e = planFirst [e]

planFirst :: [Event (Now a)] -> Now (Event a)
planFirst l = checkNows l >>= \case
               Just (i,x) -> return (delay i (return x))
               Nothing -> do m <- syncIO $ newMVar Nothing
                             syncIO $ addWatch (Watch l m)
                             return (primEv (Internal m))

checkNows :: [Event (Now a)] -> Now (Maybe (Time,a))
checkNows l = checkAll l >>= maybe (return Nothing) (\(t,a) -> Just . (t,) <$> a)

checkAll :: [Event a] -> Now (Maybe (Time,a))
checkAll []      = return Nothing
checkAll (e : t) = 
     evNow e >>= \case
        Just x  -> return (Just x)
        Nothing -> checkAll t



update :: IO ()
update = 
  do --putStrLn "starting round!"
     i <- takeMVar globalNextRound
     putMVar globalNextRound (i + 1)
     takeMVar globalLock
     takeMVar globalCurRound 
     putMVar globalCurRound i
     w <- takeMVar globalWatches 
     putMVar globalWatches []
     mapM_ takeMVarWatch w
     --putStrLn "updating watches!"
     mapM_ updateWatch w 
     --putStrLn "done watches!"
     mapM_ readMVarWatch w
     putMVar globalLock ()
     -- putStrLn "ending round!"

  where updateWatch x@(Watch l m) = forkIO $
               runNow' (checkNows l) >>= \case 
                      Just x  -> putMVar m (Just x)
                      Nothing -> do putMVar m Nothing
                                    addWatch x
                                    

        takeMVarWatch (Watch _ m) = takeMVar m >> return ()
        readMVarWatch (Watch _ m) = readMVar m >> return ()
        
addWatch x = do l <- takeMVar globalWatches
                putMVar globalWatches (x : l)


globalNextRound :: MVar Integer
globalNextRound = unsafePerformIO $ newMVar 1
{-# NOINLINE globalNextRound #-}


globalWatches :: MVar [Watch]
globalWatches = unsafePerformIO $ newMVar []
{-# NOINLINE globalWatches #-}

globalCurRound :: MVar Integer
globalCurRound = unsafePerformIO $ newMVar 0
{-# NOINLINE globalCurRound #-}

globalLock :: MVar ()
globalLock = unsafePerformIO $ newMVar ()
{-# NOINLINE globalLock #-}


globalLoop = forever $ 
             do waitForSignal globalFlag
                update
                

globalFlag :: Flag
globalFlag = unsafePerformIO $ 
                do f <- newFlag
                   forkIO $ globalLoop
                   return f
{-# NOINLINE globalFlag #-}


instance Functor Event where
  fmap = liftM

instance Applicative Event where
  pure = return
  (<*>) = ap


