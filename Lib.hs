{-# LANGUAGE GADTs, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances #-}

module Lib where

import FRPNow 
import Control.Applicative
import Control.Monad hiding (when,until)

import Debug.Trace
import Prelude hiding (until)

step :: a -> Event (Behaviour a) -> Behaviour a
step a s = pure a `switch` s

toBehaviour :: Event (Behaviour a) -> Behaviour (Maybe a)
toBehaviour e = Nothing `step` fmap (fmap Just) e

planP :: Event (Behaviour a) -> Behaviour (Event a)
planP = whenJust . toBehaviour

getNow :: Event a -> Behaviour (Maybe a)
getNow e = pure Nothing `switch` fmap (pure . Just) e

firstObs :: Event a -> Event b -> Behaviour (Event (Either a b))
firstObs l r = whenJust $ combineMaybe <$> getNow l <*> getNow r where
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


type BehaviourEnd x a = (Behaviour x, Event a)

zipBE :: (a -> b -> b) -> BehaviourEnd a x -> Behaviour b -> Behaviour b
zipBE f (bx,e) b = (f <$> bx <*> b) `switch` fmap (const b) e

(.:) :: BehaviourEnd a x -> Behaviour [a] -> Behaviour [a]
(.:) = zipBE (:)



class EventLike e where
  plan :: e (Behaviour a) -> Behaviour (e a)

instance EventLike Event where
  plan = planP



newtype BehaviourTrans m a = BT { runBT :: Behaviour (m a) }

instance (EventLike m, Monad m) => Monad (BehaviourTrans m) where
  return x = lift (return x)
  (BT m) >>= f  = BT $ 
      do x <- m
         fmap join $ plan (liftM (runBT . f) x)

lift m = BT $ pure m

cur :: Monad m => Behaviour a -> BehaviourTrans m a 
cur b = BT (return <$> b)

data UntilB x a 
   = Done a
   | Until (Behaviour x) (Event a)

instance Functor (UntilB x)     where fmap = liftM
instance Applicative (UntilB x) where pure = return ; (<*>) = ap

instance Monad (UntilB x) where
  return = Done
  Done x    >>= f = f x
  Until b e >>= f = Until (b `switch` be) ee
    where fe = fmap (next . f) e
          be = fe >>= fst
          ee = fe >>= snd

next :: UntilB x a -> (Event (Behaviour x), Event a)
next (Done a)      = (never, pure a)
next (Until b' e') = (pure b',e')

instance EventLike (UntilB x) where
  plan (Done a) = fmap Done a
  plan (Until x a) = fmap (Until x) (plan a)

type Until x a = BehaviourTrans (UntilB x) a

until :: Behaviour x -> Event a -> Until x a
until x a = lift (Until x a)



