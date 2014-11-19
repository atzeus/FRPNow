{-# LANGUAGE LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Syntactic.Time where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Data.IORef 
import Control.Monad
import System.IO.Unsafe
import Syntactic.ConcFlag

{-

-- non-shared events

data Event a 
  = Primitive (PrimEv a)
  | Ret a
  | forall x. Bind (Event x) (x -> Event a) 

instance Monad Event where
  return   = Ret
  (>>=) = Bind


evPresent :: Event a -> Present (Maybe a)
evPresent e = getEv <$> updateEv e

updateEv :: Event a -> Present (Event a)
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
  

newEv :: EventSyntax a -> Event a
newEv s = unsafePerformIO $ Ev <$> newMVar (-1,s)
{-# NOINLINE newEv #-}

evPresent :: Event a -> Present (Maybe a)
evPresent e = updateEv e >> getEv e

getEv :: Event a -> Present (Maybe a)
getEv (Ev e) = 
  do (i,s) <- syncIO $ readMVar e
     case s of
       Ret x -> return (Just x)
       SameAs b -> getEv b
       _        -> return Nothing


updateEv :: Event a -> Present (EventSyntax a)
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
    SameAs b     -> updateEv b >>= return . \case 
                      SameAs b' -> SameAs b'
                      _         -> s
    Bind e f     -> evPresent e >>= \case 
                     Just x -> do let v = f x
                                  updateEv v
                                  return (SameAs v)
                     _    -> return (Bind e f)
    _ -> return s


-- end shared events


data PrimEv a = External (IORef (Maybe a))
              | Internal (MVar (Maybe a))

newtype Present a = Present { runPresent' :: IO a } deriving (Functor, Applicative, Monad)

getPrimEv :: PrimEv a -> Present (Maybe a)
getPrimEv (External r) = Present $ readIORef r
getPrimEv (Internal r) = Present $ readMVar r

getRound :: Present Int
getRound = Present $ readMVar globalRound


data Watch = forall a. Watch { evs :: [Event (Present a)], mvar :: MVar (Maybe a)}

runPresent (Present p) = 
  do 
     takeMVar globalLock
     p
     globalFlag `seq` putMVar globalLock ()


asyncIO :: IO a -> Present (Event a)
asyncIO m = syncIO $ 
      do r <- newIORef Nothing
         forkIO $ m >>= setVal r
         return (primEv (External r))

setVal r a = 
  do takeMVar globalLock
     writeIORef r (Just a)
     putMVar globalLock ()
     signal globalFlag
              

syncIO :: IO a -> Present a
syncIO m = Present m

planFirst :: [Event (Present a)] -> Present (Event a)
planFirst l = checkAll l >>= \case
               Just x -> return (return x)
               Nothing -> do m <- syncIO $ newMVar Nothing
                             syncIO $ addWatch (Watch l m)
                             return (primEv (Internal m))
checkAll :: [Event (Present a)] -> Present (Maybe a)
checkAll []      = return Nothing
checkAll (e : t) = 
     evPresent e >>= \case
        Just x  -> x >>= return . Just
        Nothing -> checkAll t

planIO :: Event (Present a)-> Present (Event a)
planIO e = planFirst [e]

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

  where updateWatch x@(Watch l m) = forkIO $
               runPresent' (checkAll l) >>= \case 
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


