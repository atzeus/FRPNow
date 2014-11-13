module Base.TryReadMVar where

import Control.Concurrent.MVar


-- this is actually in the standard library since 4.7.0.0, but here for versions which do
-- not have that
tryReadMVar :: MVar a -> IO (Maybe a)
tryReadMVar m = 
  do v <- tryTakeMVar m
     case v of
       Just x -> putMVar m x >> return (Just x)
       Nothing -> return Nothing
                     
