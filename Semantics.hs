{-# LANGUAGE  TypeSynonymInstances #-}
module Semantics where

import Control.Applicative
import Control.Monad.Fix
import Data.Maybe

type Time = Double

type Behaviour a = Time -> a
type Event a = (Time,a)

-- reader monad
instance Monad ((->) r) where
    return = const
    f >>= k = \ r -> k (f r) r

instance MonadFix Behaviour where
   mfix f = \r -> let a = f a r in a

inf = 1/0

instance Monad Event where -- writer monad
  return a = (-inf,a)
  (t,a) >>= f = (max t t2, a)
                where (t2,a) = f a

switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch b (ts,b2) t 
   | t < ts    = b t
   | otherwise = b2 t

when f t = undefined -- ( min { t' >= t | f t }, ())

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust f t = let t2 = undefined -- min { t' >= t | isJust (f t) }
               in return (t2, fromJust $ f t2) 

plan :: Event (Behaviour a) -> Behaviour (Maybe a)
plan (te,f) = \tb -> if tb >= te
                     then Just (f tb)
                     else Nothing


data World

type Future = Behaviour World -- Time -> World -- Sausage
type Now a = Behaviour (Future -> (a,Future))

act :: IO a -> Now (Event a)
act  = undefined

runFRP :: Now (Event a) -> IO a
runFRP = undefined



