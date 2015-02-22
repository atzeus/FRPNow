{-# LANGUAGE FlexibleInstances , MultiParamTypeClasses,GADTs, TypeOperators, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances #-}

module Lib.Lib where


import Impl.FRPNow
import Swap
import Control.Applicative
import Control.Monad hiding (when)

import Prelude hiding (until)
import Swap

instance Swap Behavior Event  where swap e = whenJust (Nothing `step` fmap (fmap Just) e)

plan :: Swap b Event => Event (b a) -> b (Event a)
plan = swap


cur :: Monad m => Behavior a -> (Behavior :. m) a
cur = liftLeft


step :: a -> Event (Behavior a) -> Behavior a
step a s = pure a `switch` s

when :: Behavior Bool -> Behavior (Event ())
when b = whenJust $ choose <$> b where
  choose True = Just ()
  choose False = Nothing


occ :: Event a -> Behavior (Maybe a)
occ e = Nothing `step` fmap (pure . Just) e

first :: Event a -> Event a -> Behavior (Event a)
first l r = whenJust (occ r `switch` (pure . Just <$> l))

data Race l r = Tie l r
              | L l
              | R r

race :: Event a -> Event b -> Behavior (Event (Race a b))
race l r = whenJust (outcome <$> occ l <*> occ r) where
  outcome Nothing  Nothing  = Nothing
  outcome (Just x) Nothing  = Just (L x)
  outcome Nothing  (Just y) = Just (R y)
  outcome (Just x) (Just y) = Just (Tie x y)

hasOccured :: Event a -> Behavior Bool
hasOccured e = pure False `switch` (pure True <$ e)

change :: Eq a => Behavior a -> Behavior (Event ())
change b = do v <- b ;
              when ((v /=) <$> b) 

changeVal :: Eq a => Behavior a -> Behavior (Event a)
changeVal b = do v <- b ;
                 whenJust (notSame v <$> b)
  where notSame v v' | v /= v'   = Just v'
                     | otherwise = Nothing


becomesTrue :: Behavior Bool -> Behavior (Event ())
becomesTrue b = do v <- b
                   if v
                   then do e <- when (not <$> b)
                           join <$> plan (when b <$ e)
                   else when b

sampleUntil :: Eq a => Behavior a -> Event () -> Behavior (Event [a])
sampleUntil b end  = loop [] where
  loop ss = do s <- b
               let ss' = s : ss
               e <- hasOccured end
               if e then return (pure (reverse ss'))
               else do c <- change b
                       join <$> plan (loop ss' <$ c)  
                    

prev :: Eq a => a -> Behavior a -> Behavior (Behavior a)
prev i b = loop i where
 loop i = do c   <- b
             e   <- when ((/= c) <$> b)
             e'  <- plan (loop c <$ e)
             return (pure i `switch` e')

snapshot :: Event a -> Behavior b -> Behavior (Event (a,b))
snapshot e b = plan $ (\x -> (x,) <$> b) <$> e

(<@>) :: Behavior (a -> b) -> Event a -> Behavior (Event b)
b <@> e = plan $ fmap (\x -> b <*> pure x) e

(<@) :: Behavior a -> Event b -> Behavior (Event a)
b <@ e = plan $ b <$ e

(.@) :: Behavior a -> Event x -> Behavior (Event a)
b .@ e = (const <$> b) <@> e


zipBE :: (a -> b -> b) -> BehaviorEnd a x -> Behavior b -> Behavior b
zipBE f (bx `Until` e) b = (f <$> bx <*> b) `switch` fmap (const b) e

(.:) :: BehaviorEnd a x -> Behavior [a] -> Behavior [a]
(.:) = zipBE (:)


data BehaviorEnd x a = Until { behavior :: Behavior x, end ::  Event a }

instance Monad (BehaviorEnd x) where
  return x = pure (error "ended!") `Until` pure x
  (b `Until` e) >>= f  = 
     let v = f <$> e 
         b' = b `switch` (behavior <$> v)
         e' = v >>= end
     in b' `Until` e'


instance (Monad b, Swap b Event) => Swap b (BehaviorEnd x) where
  swap (Until b e) = liftM (Until b) (plan e)


until :: (Monad b, Swap b (BehaviorEnd x)) =>
          Behavior x -> b (Event a) -> (b :. BehaviorEnd x) a
until b e = liftLeft e >>= liftRight . (b `Until`)

  

instance Functor (BehaviorEnd x) where fmap = liftM
instance Applicative (BehaviorEnd x) where pure = return ; (<*>) = ap
          
showChanges :: (Eq a, Show a) => Behavior a -> Now ()
showChanges b = loop where
 loop = do v <- cur b
           unsafeSyncIO $ putStrLn (show v)
           e <- cur $ change b
           plan (loop <$ e)
           return ()
