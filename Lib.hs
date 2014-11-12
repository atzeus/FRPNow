{-# LANGUAGE NoMonomorphismRestriction,FlexibleInstances , MultiParamTypeClasses,GADTs, TypeOperators, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances #-}

module Lib where

import FRPNow 
import Control.Applicative
import Control.Monad hiding (when,until)

import Debug.Trace
import Prelude hiding (until)
import FunctorCompose

type Behaviour2 = (Behaviour :. Behaviour)


getNow :: Event a -> Behaviour (Maybe a)
getNow e = pure Nothing `switch` fmap (pure . Just) e

firstObs :: Event a -> Event b -> (Behaviour :. Event) (Either a b)
firstObs l r = Comp $ whenJust $ combineMaybe <$> getNow l <*> getNow r where
  combineMaybe _  (Just r) = Just (Right r)
  combineMaybe (Just l) _  = Just (Left l)
  combineMaybe _ _         = Nothing


when :: Behaviour Bool -> Behaviour (Event ())
when b = whenJust $ choose <$> b where
  choose True = Just ()
  choose False = Nothing


(<@>) :: Behaviour (a -> b) -> Event a -> Behaviour (Event b)
b <@> e = plan $ fmap (\x -> b <*> pure x) e

(.@) :: Behaviour a -> Event x -> Behaviour (Event a)
b .@ e = (const <$> b) <@> e


zipBE :: (a -> b -> b) -> BehaviourEnd a x -> Behaviour b -> Behaviour b
zipBE f (BehaviourEnd bx e) b = (f <$> bx <*> b) `switch` fmap (const b) e

(.:) :: BehaviourEnd a x -> Behaviour [a] -> Behaviour [a]
(.:) = zipBE (:)

class Cur b where cur :: Behaviour a -> b a

instance Cur Behaviour where cur = id
instance (Cur l, Functor l, Monad r) => Cur (l :. r) where cur = liftFL . cur 

class Wait e where
  wait :: Event a -> e a

instance Wait Event where
  wait = id

instance (Wait r, Monad f) => Wait (f :. r) where
  wait = liftFR . wait

later :: FlipF Event f => Event (f a) -> f (Event a)
later = flipF

delay :: Functor f => (f :. Event) x -> (f :. Event) (Event x)
delay  = Comp . fmap return . decomp

data BehaviourEnd x a = BehaviourEnd { behaviour :: Behaviour x, end ::  Event a }

instance Monad (BehaviourEnd x) where
  return x = BehaviourEnd (pure undefined) (return x)
  m >>= f  = let v = fmap f (end m) 
                 b = behaviour m `switch` fmap behaviour v
                 e = v >>= end
              in BehaviourEnd b e

instance FlipF (BehaviourEnd x) Behaviour where
  flipF (BehaviourEnd b e) = BehaviourEnd b <$> plan e

until :: (Monad f, Functor f, FlipF (BehaviourEnd x) f, Cur f) => 
         Behaviour x -> Behaviour (Event b) -> (f :. BehaviourEnd x) b
until b e = do ev <- cur e 
               liftFR (BehaviourEnd b ev)




