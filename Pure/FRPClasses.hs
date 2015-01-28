{-# LANGUAGE ConstraintKinds, MultiParamTypeClasses, FunctionalDependencies #-}
module FRPClasses where

import Control.Monad

type Event e = MonadPlus e

never :: Event m => m a
never = mzero

(.|.) :: Event m => m a -> m a -> m a
(.|.) = mplus


class (Event e, Monad b) => 
      Behavior b e | b -> e where
  switch   :: b a -> e (b a) -> b a
  whenJust :: b (Maybe a) -> b (e a)
  

