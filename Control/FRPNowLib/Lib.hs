{-# LANGUAGE NoMonomorphismRestriction,FlexibleInstances , MultiParamTypeClasses,GADTs, TypeOperators, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances #-}

module Control.FRPNowLib.Lib where

import Control.FRPNowImpl.Event
import Control.FRPNowImpl.Behaviour
import Control.Applicative
import Control.Monad hiding (when,until)

import Prelude hiding (until)
import Control.Monad.Swap

type Behaviour2 = (Behaviour :. Behaviour)
instance Swap Now Behaviour where swap n = return (n >>= curIO)
instance Swap Event Now where swap = planIO
instance Swap Event Behaviour where swap e = whenJust (Nothing `step` fmap (fmap Just) e)

plan = swap

class Monad b => Cur b where cur :: Behaviour a -> b a
instance Cur Now where cur = curIO
instance Cur Behaviour where cur = id
instance (Cur l, Monad l, Monad r, Swap r l) => Cur (l :. r) where cur = liftLeft . cur 

class Monad e => Wait e where waitEv :: Event a -> e a
instance Wait Event where waitEv = id
instance (Wait r, Monad l, Swap r l) => Wait (l :. r) where waitEv = liftRight . waitEv


wait :: (Wait m, Cur m) => Behaviour (Event b) -> m b
wait b = cur b >>= waitEv

class Monad e=>  DoIO e where
  async :: IO a -> e (Event a)

instance DoIO Now where
  async = asyncIO

instance DoIO (Behaviour :. Now) where
  async = liftRight . async


waitIO :: (DoIO l, Wait r,  Swap r l) => IO a -> (l :. r) a
waitIO m = liftLeft (async m) >>= waitEv



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
delay  = close . fmap return . open

data BehaviourEnd x a = BehaviourEnd { behaviour :: Behaviour x, end ::  Event a }

instance Monad (BehaviourEnd x) where
  return x = BehaviourEnd (pure undefined) (return x)
  m >>= f  = let v = fmap f (end m) 
                 b = behaviour m `switch` fmap behaviour v
                 e = v >>= end
              in BehaviourEnd b e

instance Swap (BehaviourEnd x) Behaviour where
  swap (BehaviourEnd b e) = BehaviourEnd b <$> plan e

until :: (Cur f, Swap (BehaviourEnd x) f) => 
         Behaviour x -> Behaviour (Event b) -> (f :. BehaviourEnd x) b
until b e = do ev <- cur e 
               liftRight (BehaviourEnd b ev)

instance Functor (BehaviourEnd x) where fmap = liftM
instance Applicative (BehaviourEnd x) where pure = return ; (<*>) = ap



