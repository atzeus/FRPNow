module Semantics where

import Control. Applicative

type Behaviour a = Time -> a
type Event a = (Time,a)

-- reader monad
instance Monad (Behaviour a) -- reader monad
  pure = const
  m >>= f = \t -> f (m t) t

instance MonadFix (Behaviour a) where
  mfix = fix

instance Monad (Event a) -- writer monad
  return a = (-inf,a)
  fmap f (t,a) = (t, f a)
  join (t,(t2,a)) = (max t t2, a)

switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch b (ts,b2) t 
   | t < ts    = b t
   | otherwise = b2 t

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust f t = let t2 = magicAnalyze (fmap isJust f) t
               in return (t2, fromJust $ f t2) 

plan :: Event (Behaviour a) -> Behaviour (Event a)
plan (te,f) = \tb -> let t = max te tb
                     in (t, f t)

magicAnalyze :: Behaviour Bool -> Behaviour Time
magicAnalyze = undefined
-- given a behaviour f and a time t1, find the time t2 , with
-- t2 >= t1, such that t2 is the minimal time such that 
-- such that f t2 is True (+ continuous time nastiness)


type Future = Behaviour World -- Time -> World -- Sausage
type Now = Behaviour (Future -> (a,Future))

act :: IO a -> Now (Event a)
act  = undefined

runFRP :: Now (Event a) -> IO a
runFRP = undefined



