module Util.ConcList(ConcList,newConcList,addConcList, takeNonEmpty) where

import Control.Concurrent.MVar

-- list for multiple producers, single reader

data ConcList a = ConcList { updateLock :: MVar (), list :: MVar [a] }

newConcList :: IO (ConcList a)
newConcList = do l <- newMVar ()
                 r <- newEmptyMVar
                 return (ConcList l r)

addConcList :: a -> ConcList a -> IO ()
addConcList v (ConcList l r) = 
  do takeMVar l
     s <- tryTakeMVar r
     case s of
       Just l  -> putMVar r (v : l)
       Nothing -> putMVar r [v]
     putMVar l ()

takeNonEmpty :: ConcList a -> IO [a]
takeNonEmpty (ConcList l r) = takeMVar r
