module TryReadMVar where

import Control.Concurrent.MVar

tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar m = 
  do v <- tryTakeMVar m
     case v of
       Just x -> putMVar m x >> return (Just x)
       Nothing -> return Nothing
                     
