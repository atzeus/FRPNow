{-# LANGUAGE  TypeSynonymInstances, TypeOperators #-}
module Semantics where

import Control.Applicative
import Control.Monad.Fix
import Data.Maybe

type Time = Double
inf = 1/0

type Behaviour a = Time -> a
data Event a = a :@ Time


instance Monad Event where -- writer monad
  return a = a :@ (-inf)
  (a :@ t) >>= f = b :@ max t t' where (b :@ t') = f a

switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch b (s :@ ts) t = if t < ts then b t else s t


whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust f t = let t' = undefined -- min { t' >= t | isJust (f t') }
               in fromJust (f t') :@ t'

{- reader monad
   this monad & monadfix is listed in the
   standard libraries as Monad ((->) r) 
   the only difference is that r = time

instance Monad Behaviour where
    return = const
    f >>= k = \ r -> k (f r) r

instance MonadFix Behaviour where
   mfix f = \t -> let a = f a t in a
-}

data World

type Future = Behaviour World -- Time -> World -- Sausage
type Now a = Behaviour (Future -> (a,Future))

act :: IO a -> Now (Event a)
act  = undefined

runFRP :: Now (Event a) -> IO a
runFRP = undefined



