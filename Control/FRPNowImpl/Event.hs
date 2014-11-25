{-# LANGUAGE LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.Event(Event,never,evNow,Now,runNow,syncIO,asyncIO,first,planIO, planFirst, RoundNr,getRound,checkAll) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef 
import Control.Monad
import System.IO.Unsafe
import Control.FRPNowImpl.ConcFlag

{-

-- non-shared events

data Event a 
  = Primitive (PrimEv a)
  | Ret a
  | forall x. Bind (Event x) (x -> Event a) 

instance Monad Event where
  return   = Ret
  (>>=) = Bind


evNow :: Event a -> Now (Maybe a)
evNow e = getEv <$> updateEv e

updateEv :: Event a -> Now (Event a)
updateEv e = case e of
  Primitive ev -> getPrimEv ev >>= return . \case 
                   Just x  -> Ret x
                   Nothing -> e
  Bind e f     -> updateEv e >>= \case 
                    Ret x -> updateEv (f x)
                    e'    -> return (Bind e' f)
  _ -> return e



getEv :: Event a -> Maybe a
getEv (Ret x) = Just x
getEv  _      = Nothing

primEv = Primitive

-- end non-shared events
-}


-- start shared events

primEv = newEv . Primitive

data EventSyntax a 
  = Primitive (PrimEv a)
  | SameAs (Event a)
  | Ret a
  | forall x. Bind (Event x) (x -> Event a) 

type RoundNr = Int
newtype Event a = Ev (MVar (RoundNr, EventSyntax a))

instance Monad Event where
  return x = newEv (Ret x)
  m >>= f  = newEv (Bind m f)
  
never = newEv (Primitive Never)

newEv :: EventSyntax a -> Event a
newEv s = unsafePerformIO $ Ev <$> newMVar (-1,s)
{-# NOINLINE newEv #-}

evNow :: Event a -> Now (Maybe a)
evNow e = updateEv e >> getEv e

getEv :: Event a -> Now (Maybe a)
getEv (Ev e) = 
  do (i,s) <- syncIO $ readMVar e
     case s of
       Ret x -> return (Just x)
       SameAs b -> getEv b
       _        -> return Nothing


updateEv :: Event a -> Now (EventSyntax a)
updateEv (Ev e) =
  do (i,s) <- syncIO $ takeMVar e
     j     <- getRound
     if i == j 
     then syncIO (putMVar e (i,s)) >> return s
     else do s' <- update' s
             syncIO $ putMVar e (j,s')
             return s' where
  update' s = case s of
    Primitive ev -> getPrimEv ev >>= return . \case 
                     Just x  -> Ret x
                     Nothing -> s
    SameAs b     -> updateEv b >>= \case 
                      SameAs b' -> SameAs <$> getDeepest b'
                      _         -> return s
    Bind e f     -> evNow e >>= \case 
                     Just x -> do let v = f x
                                  updateEv v
                                  SameAs <$> getDeepest v
                     _    -> do e' <- getDeepest e; return (Bind e' f)
    _ -> return s


getDeepest :: Event a -> Now (Event a)
getDeepest b@(Ev e) = 
  do (i,s) <- syncIO $ takeMVar e
     b' <- case s of
      SameAs b' -> getDeepest b'
      _         -> return b
     syncIO (putMVar e (i,s))
     return b'
     


-- end shared events


data PrimEv a = External (IORef (Maybe a))
              | Internal (MVar (Maybe a))
              | Never

newtype Now a = Now { runNow' :: IO a } deriving (Functor, Applicative, Monad)

getPrimEv :: PrimEv a -> Now (Maybe a)
getPrimEv (External r) = Now $ readIORef r
getPrimEv (Internal r) = Now $ readMVar r
getPrimEv Never        = return Nothing

getRound :: Now Int
getRound = Now $ readMVar globalRound

data WatchEv a = WatchFirst [Event a]

data Watch = forall a. Watch { evs :: [Event (Now a)], mvar :: MVar (Maybe a)}

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
      do r <- newIORef Nothing
         forkIO $ m >>= setVal r
         return (primEv (External r))
 where setVal r a = 
        do takeMVar globalLock
           writeIORef r (Just a)
           putMVar globalLock ()
           signal globalFlag
              

syncIO :: IO a -> Now a
syncIO m = Now m

first :: [Event a] -> Now (Event a)
first e = planFirst (map (fmap pure) e)

planIO :: Event (Now a) -> Now (Event a)
planIO e = planFirst [e]

planFirst :: [Event (Now a)] -> Now (Event a)
planFirst l = checkNows l >>= \case
               Just x -> return (return x)
               Nothing -> do m <- syncIO $ newMVar Nothing
                             syncIO $ addWatch (Watch l m)
                             return (primEv (Internal m))

checkNows :: [Event (Now a)] -> Now (Maybe a)
checkNows l = checkAll l >>= maybe (return Nothing) (Just <$>)

checkAll :: [Event a] -> Now (Maybe a)
checkAll []      = return Nothing
checkAll (e : t) = 
     evNow e >>= \case
        Just x  -> return (Just x)
        Nothing -> checkAll t



update :: IO ()
update = 
  do --putStrLn "starting round!"
     takeMVar globalLock
     w <- takeMVar globalWatches 
     putMVar globalWatches []
     mapM_ takeMVarWatch w
     --putStrLn "updating watches!"
     mapM_ updateWatch w 
     --putStrLn "done watches!"
     mapM_ readMVarWatch w
     i <- takeMVar globalRound
     putMVar globalRound (i + 1)
     putMVar globalLock ()
     --putStrLn "ending round!"

  where updateWatch x@(Watch l m) = forkIO $
               runNow' (checkNows l) >>= \case 
                      Just x  -> putMVar m (Just x)
                      Nothing -> do putMVar m Nothing
                                    addWatch x
                                    

        takeMVarWatch (Watch _ m) = takeMVar m >> return ()
        readMVarWatch (Watch _ m) = readMVar m >> return ()
        
addWatch x = do l <- takeMVar globalWatches
                putMVar globalWatches (x : l)


globalRound :: MVar Int
globalRound = unsafePerformIO $ newMVar 0
{-# NOINLINE globalRound #-}


globalWatches :: MVar [Watch]
globalWatches = unsafePerformIO $ newMVar []
{-# NOINLINE globalWatches #-}

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


