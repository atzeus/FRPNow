{-# LANGUAGE FlexibleInstances , MultiParamTypeClasses,GADTs, TypeOperators, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances #-}

module Lib.Lib where


import Impl.GTKFRPNow
import Swap
import Control.Applicative
import Control.Monad hiding (when)
import Prelude hiding (until)
import Swap
import Debug.Trace




cur :: Monad m => Behavior a -> (Behavior :. m) a
cur = liftLeft


plan :: Swap b Event => Event (b a) -> b (Event a)
plan = swap

planNow :: Event (Now a) -> Now (Event a)
planNow = plan



prev :: Eq a => a -> Behavior a -> Behavior (Behavior a)
prev i b = do v <- b
              e <- whenJustSample ((prev v b <$) . notSame v <$> b)
              return (i `step` e)


step :: a -> Event (Behavior a) -> Behavior a
step a s = pure a `switch` s

when :: Behavior Bool -> Behavior (Event ())
when b = whenJust $ choose <$> b where
  choose True = Just ()
  choose False = Nothing


occ :: E a -> B (Maybe a)
occ e = pure Nothing `switch` ((pure . Just) <$> e)

first :: E a -> E a -> B (E a)
first l r = whenJust (occ r `switch` ((pure . Just) <$> l))

countChanges :: Eq a => Behavior a -> Behavior (Behavior Int)
countChanges b = loop 0 where
  loop :: Int -> Behavior (Behavior Int)
  loop i = do  e   <-  change b
               e'  <-  snapshot (loop (i+1)) e
               return (pure i `switch` e')

foldB :: Eq a => (b -> a -> b) -> b -> Behavior a -> Behavior (Behavior b)
foldB f i b = loop i where
  loop i = do  c   <- b
               let i' = f i c
               e   <-  change b
               e'  <-  snapshot (loop i') e
               return (pure i' `switch` e')

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

notSame v v' | v /= v'   = Just v'
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

type E = Event

plan' :: E (B a) -> B (E a)
plan' e = whenJust (pure Nothing `switch` ((Just <$>) <$> e))

prev' :: Eq a => a -> Behavior a -> Behavior (Behavior a)
prev' i b = (fst <$>) <$> foldB (\(_,p) c ->  (p,c)) (undefined,i) b


type B = Behavior

buffer :: Eq a => Int -> B a -> B (B [a])
buffer n b = foldB (\l e -> take n (e : l)) [] b

snapshot :: Behavior a -> Event () -> Behavior (Event a)
snapshot b e =  let e' = (Just <$> b) <$ e
                in whenJust (pure Nothing `switch` e')

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

-- A task monad like abstraction, similar to "Monadic FRP"

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



-- debug thing

showChanges :: (Eq a, Show a) => Behavior a -> Now ()
showChanges b = loop where
 loop = do v <- sample b
           syncIO $ traceIO (show v)
           e <- sample $ change b
           plan (loop <$ e)
           return ()
