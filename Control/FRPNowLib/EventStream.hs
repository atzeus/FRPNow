{-# LANGUAGE DeriveFunctor,FlexibleInstances,ConstraintKinds,ViewPatterns,NoMonomorphismRestriction,MultiParamTypeClasses ,FlexibleContexts,TypeOperators, LambdaCase, ScopedTypeVariables, Rank2Types, GADTs, TupleSections,GeneralizedNewtypeDeriving #-}

module Control.FRPNowLib.EventStream
 (EventStream, next, nextSim, emptyEs, merge, switchEs, singleton, fmapB, filterJusts, foldB, fold,  EventStreamM, emit,runEventStreamM)
  where

import Control.FRPNowImpl.Event
import Control.FRPNowImpl.Behaviour
import Control.FRPNowLib.Lib
import Control.Monad.Free.Reflectable
import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative
import Data.Monoid
import Control.Monad.Swap
import Prelude hiding (until)
import Debug.Trace


newtype EventStream a = Es { getEs :: Behaviour (Event [a]) }

next :: EventStream a -> Behaviour (Event a)
next e = fmap head <$> getEs e

nextSim :: EventStream a -> Behaviour (Event [a]) 
nextSim e = getEs e


-- in case of simultaneity, the left elements come first
merge :: EventStream a -> EventStream a -> EventStream a
merge l r = loop where
  loop = Es $ 
   do l' <- getEs l
      r' <- getEs r
      e <- fmap nxt <$> firstObs l' r'
      let again = getEs loop
      pure e `switch` fmap (const again) e
  nxt (Tie  l r) = l ++ r
  nxt (L    l  ) = l 
  nxt (R      r) = r 

tailEs :: EventStream a -> Behaviour (Event (EventStream a))
tailEs es = fmap (const es) <$> getEs es
               
switchEs :: EventStream a -> EventStream a -> EventStream a
switchEs l r = Es $ do r' <- fmap getEs <$> tailEs r
                       getEs (merge l r) `switch` r'

emptyEs :: EventStream a
emptyEs = Es $ return never

singleton :: Event a -> EventStream a
singleton e = Es $ pure (fmap (\x -> [x]) e) `switch` fmap (const (getEs emptyEs)) e


fmapB :: Behaviour (a -> b) -> EventStream a -> EventStream b
fmapB f es = loop where
 loop = Es $ 
   do e  <- getEs es
      e' <- plan (fmap nxt e)
      let again = getEs loop
      pure e' `switch` fmap (const again) e'
 nxt l = do fv <- f ; return (fmap fv l)

instance Functor EventStream where
  fmap f = fmapB (pure f)

filterJusts :: EventStream (Maybe a) -> EventStream a
filterJusts es = loop where
 loop = Es $
  do e <- getEs es
     ev <- join <$> plan (fmap nxt e)
     let again = getEs loop
     pure ev `switch` fmap (const again) ev
 nxt l = case catMaybes l of
            [] -> getEs loop
            l  -> return (return l)

foldB :: (Behaviour a -> b -> Behaviour a) -> 
        Behaviour a -> EventStream b -> Behaviour (Behaviour a)
foldB f i es = loop i where
 loop i = 
  do e  <- getEs es
     ev <- plan (fmap (nxt i) e)
     return (i `switch` ev)
 nxt i l = loop (foldl f i l)

fold :: (a -> b -> a) -> a -> EventStream b -> Behaviour (Behaviour a)
fold f i = foldB f' (pure i)
  where f' b x = (\b -> f b x) <$> b

newtype EmitStream x = Emit { getEmit :: Behaviour ([x], EventStream x) }

emptyEmits = Emit (return ([], emptyEs ))

singleEmit x = Emit (return ([x],emptyEs))

toEventStream :: Event (EmitStream x) -> EventStream x
toEventStream em = Es $ 
    do e <- plan (fmap (firstEv . getEmit)  em)
       let ea = join (fmap fst e)
       let es = fmap snd e
       pure ea `switch` es where
                           
   firstEv :: Behaviour ([x], EventStream x) -> Behaviour (Event [x], Behaviour (Event [x]))
   firstEv b = do (l,s) <- b
                  e <- case l of
                     [] -> getEs s 
                     l  -> return (return l)
                  return (e,getEs s)

tailEmit :: EmitStream x -> EventStream x
tailEmit (Emit b) = Es $ do (_,s) <- b
                            getEs s

switchEmit :: EmitStream x -> Event (EmitStream x) -> EmitStream x
switchEmit (Emit b) es = Emit $ 
   do (l,s) <- b
      getNow es >>= \case 
         Just eb   -> do (r,sr) <- getEmit eb; return (l ++ r,sr)
         Nothing   -> return (l, s `switchEs` toEventStream es)

data EventStreamM x a = EventStreamM { emits :: EmitStream x, eend :: Event a }

instance Monad (EventStreamM x) where
  return x = EventStreamM emptyEmits (return x)
  (EventStreamM s e) >>= f = let fv = fmap f e
                                 fs = fmap emits fv
                                 fa = fv >>= eend
                             in EventStreamM (s `switchEmit` fs) fa

emit :: (Swap (BehaviourEnd x) f, Monad f) =>  x -> (f :. EventStreamM x) ()
emit x = liftRight $ EventStreamM (singleEmit x) (return ())

instance Wait (EventStreamM x) where 
  waitEv = EventStreamM emptyEmits

instance (Monad b, Swap Event b) => Swap (EventStreamM x) b where
  swap (EventStreamM b e) = liftM (EventStreamM b) (plan e)

instance Functor (EventStreamM x) where fmap = liftM
instance Applicative (EventStreamM x) where pure = return ; (<*>) = ap

runEventStreamM :: EventStreamM x a -> (EventStream x, Event a)
runEventStreamM (EventStreamM s e) = (tailEmit s, e)
  




           









