{-# LANGUAGE LambdaCase #-}
module Control.FRPNowImpl.IVar(IVar,newIVar, writeIVar, ivarVal) where
import Control.Concurrent.MVar
import Control.Applicative
import System.IO.Unsafe

newtype IVar a = IVar (MVar a)

newIVar :: IO (IVar a)
newIVar = IVar <$> newEmptyMVar

writeIVar :: IVar a -> a -> IO ()
writeIVar (IVar m) a = 
  tryReadMVar m >>= \case
    Just x -> error "Written to IVar twice!"
    Nothing -> putMVar m a

ivarVal :: IVar a -> a
ivarVal (IVar a) = unsafePerformIO $ readMVar a

-- incorrect, but sufficient here
tryReadMVar mv = do mc <- tryTakeMVar mv
                    case mc of
                     Nothing -> return mc
                     Just v  -> putMVar mv v >> return mc


