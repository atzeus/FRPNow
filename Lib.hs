{-# LANGUAGE GADTs, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances #-}

module Lib where

import Implementation 
import Control.Applicative
import Control.Monad hiding (when,until)

import Debug.Trace
import Prelude hiding (until)

cur :: Event s a -> Behaviour s (Maybe a)
cur e = pure Nothing `switch` fmap (pure . Just) e

data Race a b = L a | R b | Tie a b

combineMaybe (Just a) (Just b) = Just (Tie a b)
combineMaybe (Just a) Nothing  = Just (L a)
combineMaybe Nothing (Just b)  = Just (R b)
combineMaybe Nothing Nothing = Nothing

race :: Event s a -> Event s b -> Behaviour s (Event s (Race a b))
race a b = whenJust $ combineMaybe <$> cur a <*> cur b

when :: Behaviour s Bool -> Behaviour s (Event s ())
when b = whenJust $ choose <$> b where
  choose True = Just ()
  choose False = Nothing

appAt :: Behaviour s (a -> b) -> Event s a -> Behaviour s (Event s b)
appAt b e = plan $ fmap (\x -> liftB b <*> pure x) e

(.@) :: Behaviour s a -> Event s x -> Behaviour s (Event s a)
b .@ e = appAt (const <$> b) e

data Until s x a 
   = Done a
   | Until (Behaviour s x) (Event s a)

instance Functor (Until s x)     where fmap f m = m >>= return . f
instance Applicative (Until s x) where pure = return ; f <*> x = do fv <- f ; xv <- x ; return (fv xv)

instance Monad (Until s x) where
  return = Done
  Done x >>= f = f x
  (Until b e) >>= f = Until (b `switch` be) ee
    where fe = fmap (next . f) e
          be = fe >>= fst
          ee = fe >>= snd

next :: Until s x a -> (Event s (Behaviour s x), Event s a)
next (Done a)      = (never, pure a)
next (Until b' e') = (pure b',e')

class BehaviourLike m where
  liftB :: Behaviour s a -> m s a

instance BehaviourLike Behaviour where
  liftB = id
 
instance BehaviourLike Now where
  liftB = liftBehaviour

class PlanMonad m where
  plan :: Event s (m s a) -> m s (Event s a)

instance PlanMonad Behaviour where
  plan = planBehaviour

instance PlanMonad Now where
  plan = planNow


-- same as Until, but also allow us to sample other behaviour using current time...
runUntilMl m =  getBehaviour <$> runUntilM' m where
  getBehaviour (Until b e) =  b
runUntilM m = get <$> runUntilM' m
  where get (Until b e) = (b,e)

newtype UntilM m x s a = UntilM {runUntilM' :: m s (Until s x a) }

instance (PlanMonad m, BehaviourLike m, Monad (m s)) => Functor (UntilM m x s)      where fmap f m = m >>= return . f
instance (PlanMonad m, BehaviourLike m, Monad (m s)) => Applicative (UntilM m x s)  where pure = return ; f <*> x = do fv <- f ; xv <- x ; return (fv xv)

instance (PlanMonad m, BehaviourLike m, Monad (m s)) => 
         Monad (UntilM m x s) where
  return = UntilM . return . Done
  (UntilM m) >>= f = UntilM $
    let f' = runUntilM' . f
    in do mu <- m
          case mu of
           Done x -> f' x
           Until b e -> do e' <- plan $ fmap f' e
                           let fe = fmap next e'
                           let be = fe >>= fst
                           let ee = fe >>= snd
                           return $ Until (b `switch` be) ee

instance (PlanMonad m, BehaviourLike m) =>  
         BehaviourLike (UntilM m x) where
  liftB b = UntilM $ liftB $ Done <$> b

until :: Monad (m s) => Behaviour s x -> Event s a -> UntilM m x s a
until b e = UntilM $ return (Until b e)

untilb :: (PlanMonad m, BehaviourLike m, Monad (m s)) => Behaviour s x -> Behaviour s (Event s a) -> UntilM m x s a
untilb b e = do ev <- liftB e; until b ev

untilbl :: (PlanMonad m, BehaviourLike m, Monad (m s)) => Behaviour s x -> Behaviour s (Event s a) -> UntilM m x s x
untilbl b e = untilb b e >> liftB b

untilB :: (PlanMonad m, BehaviourLike m, Monad (m s)) => Behaviour s x -> Behaviour s Bool -> UntilM m x s ()
untilB b e = do ev <- liftB $ when e; until b ev


type BehaviourEnd s x a = (Behaviour s x, Event s a)

zipBE :: (a -> b -> b) -> BehaviourEnd s a x -> Behaviour s b -> Behaviour s b
zipBE f (bx,e) b = (f <$> bx <*> b) `switch` fmap (const b) e

(.:) :: BehaviourEnd s a x -> Behaviour s [a] -> Behaviour s [a]
(.:) = zipBE (:)
