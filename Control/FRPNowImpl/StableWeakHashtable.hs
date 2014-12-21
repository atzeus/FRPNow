{-# LANGUAGE LambdaCase #-}
module StableWeakHashtable where

import System.Mem.StableName
import System.Mem.Weak
import Data.Hashable
import qualified Data.HashTable.IO as H

type Table a b = H.CuckooHashTable (StableName a) (Weak b)



new :: IO (Table a b)
new = H.new

insert :: Table a b  -> a -> b -> IO ()
insert t a b  = 
  do s <- makeStableName a
     bw <- mkWeakPtr b (Just $ H.delete t s)
     H.insert t s bw

lookup :: Table a b -> a -> IO (Maybe b)
lookup t k =
  do s <- makeStableName k
     v <- H.lookup t s
     case v of
       Just a -> deRefWeak a
       Nothing -> return Nothing

     
