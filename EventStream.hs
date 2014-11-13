{-# LANGUAGE FlexibleInstances,ConstraintKinds,ViewPatterns,NoMonomorphismRestriction,MultiParamTypeClasses ,FlexibleContexts,TypeOperators, LambdaCase, ScopedTypeVariables, Rank2Types, GADTs, TupleSections,GeneralizedNewtypeDeriving #-}

module EventStream
 -- (EventStream, next, nextSim, (<@@>),filterJusts,filterE, silent,merge, foldB, EventM, emit, waitFor,runEventM)
  where

import Base.FRPNow
import Lib
import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative
import Data.Monoid
import FunctorCompose
import Prelude hiding (until)


newtype EventStream a = WrapES { unwrapES :: (Behaviour :. Event) (ESH a) }

type ES a = Event (ESH a)
data ESH a = a :< ES a

next :: EventStream a -> (Behaviour :. Event) a
next e = head <$> nextSim e

-- gets all next simultanious values
nextSim :: EventStream a -> (Behaviour :. Event) [a]
nextSim es = nextSimI (unwrapES es) 

nextSimI = (>>= loop []) where
  loop l (h :< t) = cur (getNow t) >>= maybe (return l) (loop (h : l))

once :: x -> EventStream x
once x = WrapES $ return (x :< never)

silent :: EventStream a
silent = WrapES $ Comp $ pure never


fmapB ::  Behaviour (a -> b) -> EventStream a -> EventStream b
fmapB f es = WrapES $ unwrapES es >>= fmapBI f

fmapBI :: Behaviour (t -> a) -> ESH t -> (Behaviour :. Event) (ESH a)
fmapBI f (h :< t) = 
    do fv <- cur f
       t' <- plan (fmapBI f <$> t)
       return (fv h :< t')

-- associative!
switchES :: EventStream a -> EventStream a -> EventStream a
switchES l r = WrapES $ do l' <- delay (unwrapES l)
                           r' <- delay (unwrapES r) 
                           switchESI l' r' where
switchESI :: ES a -> ES a -> (Behaviour :. Event) (ESH a)
switchESI l r  =
  do c <- firstObs l r
     case c of
       Left (lh :< lt) -> (lh :<) <$> delay (switchESI lt r)
       Right r         -> pure r

filterJusts :: EventStream (Maybe a) -> EventStream a
filterJusts es = WrapES $ filterJustsI (unwrapES es) 

filterJustsI :: (Behaviour :. Event) (ESH (Maybe a)) -> (Behaviour :. Event) (ESH a) 
filterJustsI b = 
  do h :< t <- b
     let t' = filterJustsI (waitEv t)
     case h of
      Just x  -> (x :<) <$> delay t'
      Nothing -> t'

-- in case of simultaneity, the left elements come first
merge :: EventStream a -> EventStream a -> EventStream a
merge l r =  WrapES $ do l' <- delay (unwrapES l)
                         r' <- delay (unwrapES r) 
                         mergeI l' r' where

mergeI :: ES a -> ES a -> (Behaviour :. Event) (ESH a)
mergeI l r  =
   do c <- firstObs r l
      case c of
        Right  (lh :< lt) -> (lh :<) <$> delay (mergeI lt r )
        Left   (rh :< rt) -> (rh :<) <$> delay (mergeI l  rt)                   

foldB :: (Behaviour a -> b -> Behaviour a) -> 
         Behaviour a -> EventStream b -> Behaviour2 a
foldB f i es = Comp $ decomp (unwrapES es) >>= pure . foldBI f i 

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
flatES e = WrapES $ waitEv e >>= unwrapES


-- Todo: Fix this have performance see paper "Reflection without Remorse" (plug, mine)
data EventStreamEnd x a = EVE { stream :: EventStream x, esend :: Event a } 

instance Monad (EventStreamEnd x) where
   return x = EVE silent (pure x)
   (EVE s e) >>= f = let fv = fmap f e
                         vs = flatES $ fmap stream fv 
                         es = fv >>= esend
                     in EVE (s `switchES` vs)  es

instance Functor (EventStreamEnd x) where fmap = liftM
instance Applicative (EventStreamEnd x) where pure = return ; (<*>) = ap
          
flipEVE :: (Functor f, FlipF Event f) => EventStreamEnd x (f a) -> f (EventStreamEnd x a)
flipEVE (EVE b e) = EVE b <$> flipF e

instance FlipF (EventStreamEnd x) Behaviour where
  flipF = flipEVE

instance FlipF (EventStreamEnd x) IO where
  flipF = flipEVE


instance Wait (EventStreamEnd x) where
   waitEv e = EVE silent e

emitBase x = EVE (once x) (return ())



emit :: (FAM f, FlipF (EventStreamEnd x) f) => 
          x -> (f :. EventStreamEnd x) ()
emit = liftFR . emitBase

