module STMemoTime where

import Control.Monad.ST
import Data.STRef
import Control.Monad.ST.Unsafe

class LiftST m where
  liftST :: ST s a -> m s a

memoTime :: (Monad (m s), LiftST m) => (x -> m s x) -> m s x -> m s x
memoTime again m = runMemo where
  mem = unsafePerformST $ newSTRef m
  {-# NOINLINE mem #-}  
  runMemo = 
    do m <- liftST $ readSTRef mem 
       res <- m
       liftST $ writeSTRef mem (again res)
       return res
{-# NOINLINE memoTime #-}  
