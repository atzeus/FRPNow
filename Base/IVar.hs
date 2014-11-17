module Base.IVar where

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import Base.TryReadMVar
import System.IO.Unsafe
import Debug.Trace

newtype IVar a = IVar (MVar a)

newDoneIVar :: a -> IO (IVar a)
newDoneIVar a = IVar <$> newMVar a

newIVar :: IO (IVar a)
newIVar = IVar <$> newEmptyMVar

writeIVar :: IVar a -> a -> IO ()
writeIVar (IVar r) a = 
  do v <- tryReadMVar r
     case v of
      Just x -> error "Written to IVar twice!"
      Nothing -> putMVar r a

readIVar :: IVar a -> IO (Maybe a)
readIVar (IVar r) = tryReadMVar r

silentBlockVal :: IVar a -> a
silentBlockVal (IVar r) = unsafePerformIO $ takeMVar r
{-# NOINLINE silentBlockVal #-}
