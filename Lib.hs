{-# LANGUAGE NoMonomorphismRestriction,FlexibleInstances , MultiParamTypeClasses,GADTs, TypeOperators, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances #-}

module Lib where

import Base.FRPNow 
import Control.Applicative
import Control.Monad hiding (when,until)

import Prelude hiding (until)
import FunctorCompose

type Behaviour2 = (Behaviour :. Behaviour)

-- because behaviour is commutative monad?

instance FlipF IO Behaviour where flipF b = pure $ b >>= curIO
instance FlipF Event Now where flipF = planIO
instance FlipF Event Behaviour where flipF e = whenJust (Nothing `step` fmap (fmap Just) e)

class Cur b where cur :: Behaviour a -> b a
instance Cur Now where cur = curIO
instance Cur Behaviour where cur = id
instance (Cur l, Functor l, Applicative r) => Cur (l :. r) where cur = liftFL . cur 

class Wait e where waitEv :: Event a -> e a
instance Wait Event where waitEv = id
instance (Wait r, Applicative f) => Wait (f :. r) where waitEv = liftFR . waitEv


wait :: (Monad m, Wait m, Cur m) => Behaviour (Event b) -> m b
wait b = cur b >>= waitEv

class DoIO e where
  async :: IO a -> e (Event a)

instance DoIO Now where
  async = asyncIO

instance DoIO (Behaviour :. Now) where
  async = liftFR . asyncDo


waitDo :: (DoIO l, Wait r,  FAM l, FAM r,  FlipF r l) =>
          IO b -> (l :. r) b
waitDo m = liftFL (asyncDo m) >>= waitEv


plan = flipF




step :: a -> Event (Behaviour a) -> Behaviour a
step a s = pure a `switch` s

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


change :: Eq a => Behaviour a -> Behaviour (Event ())
change b = do v <- b ; when $ (/= v) <$> b

(<@>) :: Behaviour (a -> b) -> Event a -> Behaviour (Event b)
b <@> e = plan $ fmap (\x -> b <*> pure x) e

(.@) :: Behaviour a -> Event x -> Behaviour (Event a)
b .@ e = (const <$> b) <@> e


zipBE :: (a -> b -> b) -> BehaviourEnd a x -> Behaviour b -> Behaviour b
zipBE f (BehaviourEnd bx e) b = (f <$> bx <*> b) `switch` fmap (const b) e

(.:) :: BehaviourEnd a x -> Behaviour [a] -> Behaviour [a]
(.:) = zipBE (:)

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

until :: (FAM f, FlipF (BehaviourEnd x) f, Cur f) => 
         Behaviour x -> Behaviour (Event b) -> (f :. BehaviourEnd x) b
until b e = do ev <- cur e 
               liftFR (BehaviourEnd b ev)

instance Functor (BehaviourEnd x) where fmap = liftM
instance Applicative (BehaviourEnd x) where pure = return ; (<*>) = ap



