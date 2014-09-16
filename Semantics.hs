module Semantics where

import Control. Applicative

type Behaviour a = Time -> a
type Event a = (Time,a)

-- reader monad
instance Monad (Behaviour a) -- reader
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
infixl 2 .@

type SpaceTime = Behaviour World -- Time -> World -- Sausage
type Now = ReaderMonad Time (StateMonad SpaceTime)   

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust f t = let t2 = magicAnalyze (fmap isJust f) t
               in return (t2, fromJust $ f t2) 


magicAnalyze :: Behaviour Bool -> Behaviour Time
magicAnalyze = undefined
-- given a behaviour f and a time t1, find the time t2 , with
-- t2 >= t1, such that t2 is the minimal time such that 
-- such that f t2 is True (+ continuous time nastiness)

liftBehaviour :: Behaviour a -> Now a
liftBehaviour f = f <$> getTime

act :: IO a -> Now (Event a)
act  = toSpaceTimeChange 



plan :: Event (Now a) -> Now (Event a)
plan (t,n) = lift (runReader t n)



-- change spacetime by planning IO a action at the given time
toSpaceTimeChange :: IO a -> Time -> SpaceTime -> (SpaceTime, Event a) 
toSpaceTimeChange = undefined

