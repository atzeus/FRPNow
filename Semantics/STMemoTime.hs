module STMemoTime where

import Control.Monad.ST
import Data.STRef
import Control.Monad.ST.UnsafePerform
import Control.Monad.Reader.Class

class LiftST m where
  liftST :: ST s a -> m s a

memoTimeST :: (Show t, Ord t, MonadReader t (m s), Monad (m s), LiftST m) => 
            (x -> m s x) -> m s x -> m s x
memoTimeST again m = runMemo where
  mem = unsafePerformST $ newSTRef (Nothing , m)
  {-# NOINLINE mem #-}  
  runMemo = 
    do st <- liftST $ readSTRef mem
       n <- ask
       res <- case st of
        (Just (t,a),_) | t == n -> return a
                       | t > n  -> error $ "Non monotonic sampling!" ++ show (t,n)
        (_, m) -> do res <- m
                     liftST $ writeSTRef mem (Just (n,res), again res)
                     return res
       return res
{-# NOINLINE memoTimeST #-}  
