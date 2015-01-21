module Control.FRPNowImpl.MemoTime where

import Data.IORef
import System.IO.Unsafe
import Control.Monad.IO.Class

memoTime :: MonadIO m => (x -> m x) -> m x -> m x
memoTime again m = runMemo where
  mem = unsafePerformIO $ newIORef m
  {-# NOINLINE mem #-}  
  runMemo = 
    do m <- liftIO $ readIORef mem 
       res <- m
       liftIO $ writeIORef mem (again res)
       return res
{-# NOINLINE memoTime #-}  
