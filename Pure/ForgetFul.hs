{-# LANGUAGE ConstraintKinds, TypeFamilies,MultiParamTypeClasses, FlexibleInstances, TypeOperators,LambdaCase,UndecidableInstances,FunctionalDependencies #-}
module ForgetFul(primEvent, Ev, B) where

import EventM
import BehaviorM
import Memory
import FRPClasses
import Control.Monad
import Control.Monad.Reader.Class


class Drop e where
  drop :: Monad m => m (e m a) -> e m a

class Var e where
  var :: e m a -> m (e m a)
  

newtype LetM m a = Let { runlet :: m a } deriving (Monad,MonadFix)

var :: (Memo e, Memory m) => e m a -> m (e m a)
var = memo

elet :: (Monad m, Snatch e) => LetM m (e m a) -> e m a
elet = snatch . runlet

ein :: Monad m => a -> LetM m a
ein = return 
