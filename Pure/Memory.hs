{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts, MultiParamTypeClasses, TypeFamilies #-}

module Memory where

import Control.Monad
import Control.Monad.Fix

class Monad m => Memory m  where
  type Ref m
  newRef   :: a -> m (Ref m a)
  writeRef :: Ref m a -> a -> m ()
  readRef  :: Ref m a -> m a


memoAgain :: (Memory m) => (x -> m x) -> m x -> m (m x)
memoAgain again m = liftM runAgain (newRef m) where
  runAgain r = 
    do m <- readRef r
       res <- m
       writeRef r (again res)
       return res


