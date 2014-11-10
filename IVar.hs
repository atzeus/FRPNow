module IVar where

import Control.Concurrent.MVar
import Control.Applicative
import Control.Monad
import TryReadMVar
import System.IO.Unsafe


newtype IVar a = IVar (MVar a)

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


valIVar :: IVar a -> a
{-# NOINLINE valIVar #-}
valIVar (IVar r) = unsafePerformIO $ takeMVar r

