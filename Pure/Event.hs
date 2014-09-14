module Pure.Event where

import Control.Monad.Reader
import Race

data Time = MinBound | Time Double | MaxBound deriving (Ord,Eq)

instance Bounded Time where
  minBound = MinBound
  maxBound = MaxBound

data Event a = Event Time a

instance Functor Event where
  fmap f (Event t a) = Event t (f a)

instance Monad Event where
  return = Event minBound
  (Event t a) >>= f = Event (max t t2) b
    where Event t2 b = f a

never :: Event a
never = Event maxBound undefined







