{-# LANGUAGE NoMonomorphismRestriction,FlexibleInstances , MultiParamTypeClasses,GADTs, TypeOperators, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances #-}

module Control.FRPNowLib.Lib where


import Control.FRPNowImpl.FRPNow
import Control.Applicative
import Control.Monad hiding (when)

import Prelude hiding (until)
import Control.Monad.Swap

type Behavior2 = (Behavior :. Behavior)
instance Swap Now Behavior where swap n = return (n >>= curIO)
instance Swap Event Now where swap = planIO
instance Swap Event Behavior where swap e = whenJust (Nothing `step` fmap (fmap Just) e)

plan :: Swap Event b => Event (b a) -> b (Event a)
plan = swap

class Monad b => Cur b where cur :: Behavior a -> b a
instance Cur Now where cur = curIO
instance Cur Behavior where cur = id
instance (Cur l, Monad l, Monad r, Swap r l) => Cur (l :. r) where cur = liftLeft . cur 

class Monad e => Wait e where waitEv :: Event a -> e a
instance Wait Event where waitEv = id
instance (Wait r, Monad l, Swap r l) => Wait (l :. r) where waitEv = liftRight . waitEv


wait :: (Wait m, Cur m) => Behavior (Event b) -> m b
wait b = cur b >>= waitEv



class Monad e => DoIO e where
  async :: IO a -> e (Event a)

instance DoIO Now where
  async = asyncIO

instance DoIO (Behavior :. Now) where
  async = liftRight . async


waitIO :: (DoIO l, Wait r,  Swap r l) => IO a -> (l :. r) a
waitIO m = liftLeft (async m) >>= waitEv



step :: a -> Event (Behavior a) -> Behavior a
step a s = pure a `switch` s

getNow :: Event a -> Behavior (Maybe a)
getNow e = pure Nothing `switch` fmap (pure . Just) e

hasOccured :: Event a -> Behavior Bool
hasOccured e = pure False `switch` (pure True <$ e)

data First a b = L a | R b | Tie a b

firstObs :: Event a -> Event b -> Behavior (Event (First a b))
firstObs l r = whenJust $ combineMaybe <$> getNow l <*> getNow r where
  combineMaybe (Just l') (Just r') = Just (Tie l' r')
  combineMaybe (Just l') Nothing   = Just (L   l'   )
  combineMaybe Nothing   (Just r') = Just (R      r')
  combineMaybe Nothing   Nothing   = Nothing


when :: Behavior Bool -> Behavior (Event ())
when b = whenJust $ choose <$> b where
  choose True = Just ()
  choose False = Nothing


change :: Eq a => Behavior a -> Behavior (Event a)
change b = do v <- b ; whenJust (isNot v <$> b) where
  isNot v a | v == a = Nothing
            | otherwise = Just a

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
zipBE f (BehaviorEnd bx e) b = (f <$> bx <*> b) `switch` fmap (const b) e

(.:) :: BehaviorEnd a x -> Behavior [a] -> Behavior [a]
(.:) = zipBE (:)

delay :: Functor f => (f :. Event) x -> (f :. Event) (Event x)
delay  = close . fmap return . open

noWait :: (Functor f, Swap Event f, Cur f) => (f :. Event) x -> (f :. Event) (Maybe x)
noWait m = delay m >>= cur . getNow


data BehaviorEnd x a = BehaviorEnd { behavior :: Behavior x, end ::  Event a }

instance Monad (BehaviorEnd x) where
  return x = BehaviorEnd (pure undefined) (return x)
  m >>= f  = let v = fmap f (end m) 
                 b = behavior m `switch` fmap behavior v
                 e = v >>= end
              in BehaviorEnd b e

instance (Monad b, Swap Event b) => Swap (BehaviorEnd x) b where
  swap (BehaviorEnd b e) = liftM (BehaviorEnd b) (plan e)

until :: (Cur f, Swap (BehaviorEnd x) f) => 
         Behavior x -> Behavior (Event b) -> (f :. BehaviorEnd x) b
until b e = do ev <- cur e 
               liftRight (BehaviorEnd b ev)

untilb b e = until b e >> cur b
                  

instance Functor (BehaviorEnd x) where fmap = liftM
instance Applicative (BehaviorEnd x) where pure = return ; (<*>) = ap
          
showChanges :: (Eq a, Show a) => Behavior a -> Now ()
showChanges b = loop where
 loop = do v <- cur b
           syncIO $ putStrLn (show v)
           e <- cur $ whenJust (toJust v <$> b)
           planIO (fmap (const (loop)) e)
           return ()
  where  toJust v x = if v == x then Nothing else Just x
