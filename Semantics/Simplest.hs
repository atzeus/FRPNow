{-# LANGUAGE LambdaCase #-}

import Control.Applicative
import Control.Monad

data Event m a = E { runEvent :: m (Maybe a) }

never :: Monad m => Event m a
never = E (return Nothing)

instance Monad m => Monad (Event m) where
  return x = E $ return (Just x)
  m >>= f = E $ runEvent m >>= maybe (return Nothing) (runEvent . f)

data Behavior m a = B { runBehavior :: m a }

instance Monad m => Monad (Behavior m) where
  return x = B $ return x
  m >>= f  = B $ runBehavior m >>= runBehavior . f

switch :: Monad m=>  Behavior m a -> Event m (Behavior m a) -> Behavior m a
switch b e = B $ runEvent e >>= runBehavior . fromMaybe b

class Monad m => Plan m where
  everyRound :: m (Maybe a) -> m (Event a)

whenJust :: Plan m => Behavior m (Maybe a) -> Behavior m (Event m a)
whenJust b = B $ everyRound (runBehavior b) where

