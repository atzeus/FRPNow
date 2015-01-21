{-# LANGUAGE GeneralizedNewtypeDeriving, LambdaCase #-}

import Control.Applicative
import Control.Monad
import Data.Maybe
import Control.Monad.Trans.Maybe

newtype Event m a = E (MaybeT m a) deriving Monad
runE (E m) = runMaybeT m

never :: Monad m => Event m a
never = E mzero

newtype Behavior m a = B { runB :: m a } deriving Monad

switch :: Monad m=>  Behavior m a -> Event m (Behavior m a) -> Behavior m a
switch b e = B $ runE e >>= runB . fromMaybe b

class Monad m => Plan m where
  everyRound :: m (Maybe a) -> m (Event m a)

whenJust :: Plan m => Behavior m (Maybe a) -> Behavior m (Event m a)
whenJust b = B $ everyRound (runB b)

