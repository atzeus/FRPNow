{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad
import Data.Maybe

data Event m a = E { runE :: m (Maybe a) }

never :: Monad m => Event m a
never = E (return Nothing)

instance Monad m => Monad (Event m) where
  return x = E $ return (Just x)
  m >>= f = E $ runE m >>= maybe (return Nothing) (runE . f)

data Behavior m a = B { runB :: m a }

instance Monad m => Monad (Behavior m) where
  return x = B $ return x
  m >>= f  = B $ runB m >>= runB . f

switch :: Monad m=>  Behavior m a -> Event m (Behavior m a) -> Behavior m a
switch b e = B $ runE e >>= runB . fromMaybe b

class Monad m => Plan m where
  everyRound :: m (Maybe a) -> m (Event m a)

whenJust :: Plan m => Behavior m (Maybe a) -> Behavior m (Event m a)
whenJust b = B $ everyRound (runB b) where

