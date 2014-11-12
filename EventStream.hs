{-# LANGUAGE ViewPatterns,NoMonomorphismRestriction,MultiParamTypeClasses ,FlexibleContexts,TypeOperators, LambdaCase, ScopedTypeVariables, Rank2Types, GADTs, TupleSections,GeneralizedNewtypeDeriving #-}

module EventStream
 -- (EventStream, next, nextSim, (<@@>),filterJusts,filterE, silent,merge, foldB, EventM, emit, waitFor,runEventM)
  where

import FRPNow
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
nextSim es =  unwrapES es >>= loop [] where
  loop l (h :< t) = cur (getNow t) >>= maybe (return l) (loop (h : l))


(<@@>) :: forall a b. Behaviour (a -> b) -> EventStream a -> EventStream b
(<@@>) f b  = WrapES $ unwrapES b >>= loop where
  loop (h :< t) = do v  <- cur f
                     t' <- later (loop <$> t)
                     return (v h :< t')

once :: x -> EventStream x
once x = WrapES $ return (x :< never)

silent :: EventStream a
silent = WrapES $ Comp $ pure never

switchES :: forall a. EventStream a -> EventStream a -> EventStream a
switchES e s = WrapES $ do s' <- delay (unwrapES s)
                           e' <- delay (unwrapES e)
                           loop s' e' where
  loop :: ES a -> ES a -> (Behaviour :. Event) (ESH a)
  loop s l  =
    do c <- firstObs l s
       case c of
         Left (lh :< lt) -> (lh :<) <$> delay (loop s lt)
         Right r         -> pure r



-- Todo: Does this have performance problem? see paper "Reflection without Remorse" (plug, mine)
data EventStreamEnd x a = EVE { stream :: EventStream x, esend :: Event a }

instance Monad (EventStreamEnd x) where
   return x = EVE silent (pure x)
   (EVE s e) >>= f = let fv = fmap f e
                         vs = flatES $ fmap stream fv 
                         es = fv >>= esend
                     in EVE (s `switchES` vs)  es

flatES :: Event (EventStream x) -> EventStream x
flatES e = WrapES $ wait e >>= unwrapES
               

instance FlipF (EventStreamEnd x) Behaviour where
  flipF (EVE b e) = EVE b <$> plan e

instance Wait (EventStreamEnd x) where
   wait e = EVE silent e

emitBase x = EVE (once x) (return ())

emit :: (Monad f, Functor f, FlipF (EventStreamEnd x) f) => 
          x -> (f :. EventStreamEnd x) ()
emit = liftFR . emitBase

filterJusts :: EventStream (Maybe a) -> EventStream a
filterJusts b = WrapES $ unwrapES b >>= loop where
  loop (h :< t) = 
   let t' = wait t >>= loop
   in case h of
       Just x  -> (x :<) <$> delay t'
       Nothing -> t'
                   
filterE :: (a -> Bool) -> EventStream a -> EventStream a
filterE f b = filterJusts $ pure toMaybe <@@> b
  where toMaybe x = if f x then Just x else Nothing



-- in case of simultanuaty, the left elements come first
merge :: forall a. EventStream a -> EventStream a -> EventStream a
merge l r = WrapES $ 
  do lv <- delay (unwrapES l)
     rv <- delay (unwrapES r)
     loop lv rv where
  loop :: ES a -> ES a -> (Behaviour :. Event) (ESH a)
  loop l r = do v <- firstObs r l  
                case v of
                 Right (lh :< lt) -> (lh :<) <$> delay (loop lt r )
                 Left  (rh :< rt) -> (rh :<) <$> delay (loop l  rt)



foldB :: (Behaviour a -> b -> Behaviour a) -> 
         Behaviour a -> EventStream b -> Behaviour2 a
foldB f i e = Comp . fmap behaviour . decomp $ 
                do e <- cur (decomp (unwrapES e))
                   loop i e where
    loop i e = do h :< t <- i `until` pure e
                  loop (f i h) t

fold :: (a -> b -> a) -> a -> EventStream b -> Behaviour2 a
fold f i = foldB (\x y -> f <$> x <*> pure y) (pure i)
              
parList :: EventStream (BehaviourEnd a x) -> Behaviour2 [a]
parList = foldB (flip (.:)) (pure []) 


