{-# LANGUAGE DoAndIfThenElse, FlexibleInstances , MultiParamTypeClasses,GADTs, TypeOperators, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.FRPNow.Lib
-- Copyright   :  (c) Atze van der Ploeg 2015
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
-- 
-- Utility FRPNow functions
module Control.FRPNow.Lib(
   -- * Behavior construction
   step,
   -- * Getting events from behaviors 
   when,
   change,
   becomesTrue,
   -- * Events and their ordering
   occ,
   hasOccured,
   first,
   cmpTime,
   EvOrd(..),
   -- * Fold and state
   foldB,
   sampleUntil,
   -- * Sample behaviors on events
   plan,
   snapshot,
   (<@>),
   -- * Debugging 
   traceChanges )
   
 where


import Control.FRPNow.Core
import Control.Applicative
import Control.Monad hiding (when)
import Prelude hiding (until)
import Debug.Trace


-- | Shorthand for 
-- > pure a `switch` s
step :: a -> Event (Behavior a) -> Behavior a
step a s = pure a `switch` s

-- | Like 'Control.FRPNow.whenJust' but on behaviors of type @Bool@ instead of @Maybe@. 
-- 
--  Gives the event that the input behavior is @True@
when :: Behavior Bool -> Behavior (Event ())
when b = whenJust (boolToMaybe <$> b) where
  boolToMaybe True   = Just ()
  boolToMaybe False  = Nothing


-- | Gives at any point in time the event that the input behavior changes, and the new value of the input behavior.
change :: Eq a => Behavior a -> Behavior (Event a)
change b = futuristic $ 
           do v <- b ;
              whenJust (notSame v <$> b) where
    notSame v v' | v /= v'   = Just v'
                 | otherwise = Nothing
               


-- | The resulting behavior gives at any point in time, the event that the input
-- behavior next _becomes_ true. If the input behavior is True already, the event gives the
-- time that it is True again, after first being False for a period of time.
becomesTrue :: Behavior Bool -> Behavior (Event ())
becomesTrue b = futuristic $ 
             b >>= \v -> 
              if v then (do e <- when (not <$> b)
                            join <$> plan (when b <$ e))
              else when b

-- | A fold over a behavior
foldB :: Eq a => (b -> a -> b) -> b -> Behavior a -> Behavior (Behavior b)
foldB f i b = loop i where
  loop i = do  c   <- b
               let i' = f i c
               e   <-  change b
               e'  <-  snapshot (loop i') (() <$ e)
               return (pure i' `switch` e')

-- | When sampled at a point in time t, the behavior gives an event with 
-- the list of all values of the input behavior between time t and the
-- time that the argument event occurs (including the value when the event occurs). 
sampleUntil :: Eq a => Behavior a -> Event () -> Behavior (Event [a])
sampleUntil b end  = loop [] where
  loop ss = do s <- b
               let ss' = s : ss
               e <- hasOccured end
               if e then return (pure (reverse ss'))
               else do c <- change b
                       join <$> plan (loop ss' <$ c)


-- | Convert an event into a behavior that gives 
-- @Nothing@ if the event has not occured yet, and @Just@ the value of the event if the event has already occured.
occ :: Event a -> Behavior (Maybe a)
occ e = pure Nothing `switch` ((pure . Just) <$> e)

-- | The resulting behavior states wheter the input event has already occured.
hasOccured :: Event x -> Behavior Bool
hasOccured e = False `step` (pure True <$ e)

-- | Gives the first of two events. 
-- 
-- If either of the events lies in the future, then the result will be the first of these events.
-- If both events have already occured, the left event is returned.
first :: Event a -> Event a -> Behavior (Event a)
first l r = whenJust (occ r `switch` ((pure . Just) <$> l))

-- | Compare the time of two events.
-- 
-- The resulting behavior gives an event, occuring at the same time 
-- as the earliest input event, of which the value indicates if the event where
-- simultanious, or if one was earlier. 
--
-- If at the time of sampling both event lie in the past, then 
-- the result is that they are simulatinous.
cmpTime :: Event a -> Event b -> Behavior (Event (EvOrd a b))
cmpTime l r = whenJust (outcome <$> occ l <*> occ r) where
  outcome Nothing  Nothing  = Nothing
  outcome (Just x) Nothing  = Just (LeftEarlier x)
  outcome Nothing  (Just y) = Just (RightEarlier y)
  outcome (Just x) (Just y) = Just (Simul x y)

-- | The outcome of a 'cmpTime': the events occur simultanious, left is earlier or right is earlier.
data EvOrd l r = Simul l r
               | LeftEarlier l
               | RightEarlier r

-- | Plan to sample the behavior carried by the event as soon as possible. 
-- 
-- If the resulting behavior is sampled after the event occurs,
-- then the behavior carried by the event will be sampled now.
plan :: Event (Behavior a) -> Behavior (Event a)
plan e =  whenJust 
           (pure Nothing `switch` ((Just <$>) <$> e)) 



-- | Obtain the value of the behavior at the time the event occurs
--
-- If the event has already occured when sampling the resulting behavior,
-- we sample not the past, but the current value of the input behavior.
snapshot :: Behavior a -> Event () -> Behavior (Event a)
snapshot b e =  let e' = (Just <$> b) <$ e
                in whenJust (pure Nothing `switch` e')

-- | Like 'snapshot', but feeds the result of the event to the
-- value of the given behavior at that time.
(<@>) :: Behavior (a -> b) -> Event a -> Behavior (Event b)
b <@> e = plan $ fmap (\x -> b <*> pure x) e







-- | A debug function, prints all values of the behavior to stderr, prepended with the given string.
traceChanges :: (Eq a, Show a) => String -> Behavior a -> Now ()
traceChanges s b = loop where
 loop = do v <- sample b
           sync $ traceIO (s ++ show v)
           e <- sample $ change b
           planNow (loop <$ e)
           return ()
