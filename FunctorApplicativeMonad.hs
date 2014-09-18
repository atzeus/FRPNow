{-# LANGUAGE FlexibleInstances,UndecidableInstances #-}

module FunctorApplicativeMonad(module Control.Applicative,module Control.Monad, FunctorApplicativeMonad(..)) where

import Control.Applicative
import Control.Monad hiding (when,until)

-- getting sick of copying these functor and applicative interfaces... 

class FunctorApplicativeMonad m where
 ret  :: a -> m a
 bind :: m a -> (a -> m b) -> m b

instance FunctorApplicativeMonad m => Monad m where
  return = ret
  (>>=) = bind

instance FunctorApplicativeMonad m => Functor m where
  fmap f m = bind m (ret . f)

instance FunctorApplicativeMonad m => Applicative m where
  pure = ret
  f <*> x = bind f $ \fv -> bind x $ \xv -> ret (fv xv)
