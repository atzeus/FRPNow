{-# LANGUAGE FlexibleInstances,ConstraintKinds,ViewPatterns,NoMonomorphismRestriction,MultiParamTypeClasses ,FlexibleContexts,TypeOperators, LambdaCase, ScopedTypeVariables, Rank2Types, GADTs, TupleSections,GeneralizedNewtypeDeriving #-}

module Control.FRPNowLib.EventStream
 (EventStream, next, nextSim, once, silent ,fmapB, merge, filterJusts, foldB, fold, filterB, filterE, parList, EventStreamM(..), emit ,runEventStreamM)
  where

import Control.FRPNowImpl.Event
import Control.FRPNowImpl.Behaviour
import Control.FRPNowLib.Lib
import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative
import Data.Monoid
import Control.Monad.Swap
import Prelude hiding (until)
import Debug.Trace


newtype EventStream a = WrapES { unwrapES :: (Behaviour :. Event) (ESH a) }

type ES a = Event (ESH a)
data ESH a = a :< ES a

wrap :: (Behaviour :. Event) (ESH a) -> EventStream a
wrap b = WrapES $ close $  open b >>= wrappy

next :: EventStream a -> (Behaviour :. Event) a
next e = head <$> nextSim e

-- gets all next simultanious values
nextSim :: EventStream a -> (Behaviour :. Event) [a]
nextSim es = close $ nextSimI (unwrapES es) 

nextSimI :: (Behaviour :. Event) (ESH a) -> Behaviour (Event [a])
nextSimI es = do c <- open es
                 plan (fmap (loop []) c) where
  loop l (h :< t) = cur (getNow t) >>= maybe (return (h : l)) (loop (h : l))

once :: x -> EventStream x
once x = WrapES $ return (x :< never)

silent :: EventStream a
silent = WrapES $ close $ pure never

fmapB ::  Behaviour (a -> b) -> EventStream a -> EventStream b
fmapB f es = wrap $ unwrapES es >>= fmapBI f

fmapBI :: Behaviour (t -> a) -> ESH t -> (Behaviour :. Event) (ESH a)
fmapBI f (h :< t) = 
    do fv <- cur f
       t' <- plan (fmapBI f <$> t)
       return (fv h :< t')

-- associative!
{-
switchES :: EventStream a -> EventStream a -> EventStream a
switchES l r = wrap $ do l' <- delay (unwrapES l)
                         r' <- delay (unwrapES r) 
                         switchESI l' r'

                           let rr = open (unwrapES r)
                           x  <- cur rr
                           let x' = fmap (const rr) x
                           close $ open (switchESI l' r') `switch` x' where


switchESI :: ES a -> ES a -> (Behaviour :. (Event (ESH a))
switchESI l r  = loop l where
 loop i l =
  do c <- firstObs l r
     case c of
       Left (lh :< lt) -> (lh :<) <$> delay (loop (i+1) lt)
       Right r         -> pure r

-}
-- in case of simultaneity, the left elements come first
merge :: EventStream a -> EventStream a -> EventStream a
merge l r =  wrap $ do l' <- delay (unwrapES l)
                       r' <- delay (unwrapES r) 
                       mergeI l' r' 

mergeI :: ES a -> ES a -> (Behaviour :. Event) (ESH a)
mergeI l r  =
   do c <- firstObs r l
      case c of
        Right  (lh :< lt) -> (lh :<) <$> delay (mergeI lt r )
        Left   (rh :< rt) -> (rh :<) <$> delay (mergeI l  rt)    

filterJusts :: EventStream (Maybe a) -> EventStream a
filterJusts es = wrap $ filterJustsI (unwrapES es) 

filterJustsI :: (Behaviour :. Event) (ESH (Maybe a)) -> (Behaviour :. Event) (ESH a) 
filterJustsI b = 
  do h :< t <- b
     let t' = filterJustsI (waitEv t)
     case h of
      Just x  -> (x :<) <$> delay t'
      Nothing -> t'
               

foldB :: (Behaviour a -> b -> Behaviour a) -> 
         Behaviour a -> EventStream b -> Behaviour2 a
foldB f i es = close $ open (unwrapES es) >>= pure . foldBI f i 

foldBI :: (Behaviour a -> b -> Behaviour a) -> 
         Behaviour a -> ES b -> Behaviour a
foldBI f i e = i `switch` (cont <$> e) where
  cont (h :< t) = foldBI f (f i h) t

filterB :: Behaviour (a -> Bool) -> EventStream a -> EventStream a
filterB b = filterJusts . fmapB (toMaybe <$> b) 

filterE :: (a -> Bool) -> EventStream a -> EventStream a
filterE f = filterB (pure f)
 
toMaybe :: (a -> Bool) -> a -> Maybe a
toMaybe f x = if f x then Just x else Nothing

fold :: (a -> b -> a) -> a -> EventStream b -> Behaviour2 a
fold f i = foldB (\x y -> f <$> x <*> pure y) (pure i)

parList :: EventStream (BehaviourEnd a x) -> Behaviour2 [a]
parList = foldB (flip (.:)) (pure []) 

instance Functor EventStream where
  fmap f = fmapB (pure f)

flatES :: Event (EventStream x) -> EventStream x
flatES e = wrap $ waitEv e >>= unwrapES


repeatEV ::  Now (Event a) -> Now (EventStream a)
repeatEV m = do x <- loop
                return (WrapES $ close $ wrappy x)  where
  loop = do x <- m
            x' <- planIO (fmap (\a -> (a :<) <$> loop) x)
            return x' 
            

wrappy :: Event (ESH a) -> Behaviour (Event (ESH a))
wrappy e = pure e `switch` fmap loop e where
  loop :: ESH a -> Behaviour (Event (ESH a))
  loop (_ :< e) = pure e `switch` fmap loop e 


data EventStreamM b x a = EVE {fromEVE :: b (ES x, Event a)}

instance (Monad b, Cur b,Swap Event b) => Monad (EventStreamM b x) where
   return x = EVE $ return (never, return x)
   (EVE b) >>= f = EVE $ do (s,a) <- b
                            e2 <- plan $ fmap (fromEVE . f) a 
                            let b = e2 >>= snd 
                            s' <- cur $ open $ switchEVS s (e2 >>= fst)
                            return (s',b) 

instance (Cur b, Swap Event b, Monad b) => Wait (EventStreamM b x) where waitEv e = EVE $ return (never, e)                          

instance (Cur b, Swap Event b, Monad b) => Cur (EventStreamM b x) where cur b = EVE $ do x <- cur b ; return (never, return x)

emit :: Monad b => x -> EventStreamM b x ()
emit x = EVE $ return (return (x :< never), return ())

runEventStreamM :: Monad b =>  EventStreamM b x a -> b (EventStream x, Event a)
runEventStreamM (EVE b) = do (s,a) <- b
                             return (WrapES $ close (wrappy s), a)
                            
switchEVS :: ES a -> ES a -> (Behaviour :. Event) (ESH a)
switchEVS l r  = loop l where
 loop l =
  do c <- firstObs l r
     case c of
       Left (lh :< lt) -> (lh :<) <$> delay (loop lt)
       Right r         -> pure r


instance (Cur b, Monad b, Swap Event b) => Functor (EventStreamM b x) where fmap = liftM
instance (Cur b, Monad b, Swap Event b) => Applicative (EventStreamM b x) where pure = return ; (<*>) = ap
          

