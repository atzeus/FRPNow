{-# LANGUAGE FlexibleInstances,ConstraintKinds,ViewPatterns,NoMonomorphismRestriction,MultiParamTypeClasses ,FlexibleContexts,TypeOperators, LambdaCase, ScopedTypeVariables, Rank2Types, GADTs, TupleSections,GeneralizedNewtypeDeriving #-}

module Control.FRPNowLib.EventStream
 (EventStream, next, nextSim, once, silent ,fmapB, merge, filterJusts, foldB, fold, filterB, filterE, parList, EventStreamEnd(..), emit)
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

switchWrap :: (Behaviour :. Event) (ESH a) -> EventStream a
switchWrap b = WrapES $ close $  open b >>= wrappy

wrappy :: Event (ESH a) -> Behaviour (Event (ESH a))
wrappy e = pure e `switch` fmap loop e where
  loop :: ESH a -> Behaviour (Event (ESH a))
  loop (_ :< e) = pure e `switch` fmap loop e 

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
fmapB f es = switchWrap $ unwrapES es >>= fmapBI f

fmapBI :: Behaviour (t -> a) -> ESH t -> (Behaviour :. Event) (ESH a)
fmapBI f (h :< t) = 
    do fv <- cur f
       t' <- plan (fmapBI f <$> t)
       return (fv h :< t')

-- associative! _no_ reflection without remorse problem (because we forget the past)
switchES :: EventStream a -> Event (EventStream a) -> EventStream a
switchES l r = WrapES $ close $ -- notice no switchWrap!
  do ls     <- open (unwrapES l)
     let r' = fmap (open . unwrapES) r
     rs     <- join <$> plan r'
     open (switchESI ls rs) `switch` r'

switchESI :: ES a -> ES a -> (Behaviour :. Event) (ESH a)
switchESI l r  = loop l where
 loop l =
  do c <- firstObs l r
     case c of
       Left (lh :< lt) -> (lh :<) <$> delay (loop lt)
       Right r         -> pure r

-- in case of simultaneity, the left elements come first
merge :: EventStream a -> EventStream a -> EventStream a
merge l r =  switchWrap $
   do l' <- delay (unwrapES l)
      r' <- delay (unwrapES r) 
      mergeI l' r' 

mergeI :: ES a -> ES a -> (Behaviour :. Event) (ESH a)
mergeI l r  =
   do c <- firstObs r l
      case c of
        Right  (lh :< lt) -> (lh :<) <$> delay (mergeI lt r )
        Left   (rh :< rt) -> (rh :<) <$> delay (mergeI l  rt)    

filterJusts :: EventStream (Maybe a) -> EventStream a
filterJusts es = switchWrap $ filterJustsI (unwrapES es) 

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

data EventStreamEnd x a = EventStreamEnd { stream :: EventStream x, sEnd :: Event a }

instance Monad (EventStreamEnd x) where
  return x = EventStreamEnd  silent (return x)
  m >>= f  = let nxt      = fmap f (sEnd m)
                 stream'  = stream m `switchES` fmap stream nxt
                 end'     = join (fmap sEnd nxt)
             in EventStreamEnd stream' end'

emit ::  x -> EventStreamEnd x ()
emit x = EventStreamEnd (once x) (return ())

instance Swap (EventStreamEnd x) Behaviour where
  swap (EventStreamEnd b e) = EventStreamEnd b <$> plan e

instance Wait (EventStreamEnd x) where waitEv e = EventStreamEnd silent e
