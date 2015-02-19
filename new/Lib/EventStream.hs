{-# LANGUAGE DeriveFunctor,FlexibleInstances,ConstraintKinds,ViewPatterns,NoMonomorphismRestriction,MultiParamTypeClasses ,FlexibleContexts,TypeOperators, LambdaCase, ScopedTypeVariables, Rank2Types, GADTs, TupleSections,GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Lib.EventStream
-- (EventStream(..), next, nextSim, emptyEs, repeatEv, merge, switchEs, singletonEs, fmapB, filterJusts, foldB, fold, during, sampleOn, parList, scanlEv, last,filterEv, bufferStream, foldr1Ev, foldrEv, foldrSwitch, changes, EventStreamM, emit,runEventStreamM, printAll, repeatEvN)
  where

import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative hiding (empty)

import Data.Sequence hiding (reverse,scanl,take)
import Prelude hiding (until,length)
import Debug.Trace

import Swap
import FRPNow
import Lib.Lib

type Es a = Event (Eht a)
data Eht a = Eht a (Es a)

instance Functor Eht where
  fmap f (Eht h t) = Eht (f h) (fmap (fmap f) t)

newtype Stream a = Stream (Behavior (Es a))

instance Functor Stream where
  fmap f (Stream s) = Stream (fmap (fmap (fmap f)) s)

wrap :: Es a -> Behavior (Es a)
wrap e = e `step` ((\(Eht _ t) -> wrap t) <$> e) 

toChanges :: Eq a => Behavior a -> Stream a
toChanges b = Stream loop where
  loop = do h <- change b
            let nxt = Eht <$> b <*> loop
            t <- plan (nxt <$ h)
            wrap t

fromChanges :: a -> Stream a -> Behavior a
fromChanges i (Stream s) = s >>= \t -> loop (Eht i t) 
  where loop (Eht h t) = pure h `switch` (loop <$> t)

fromSwitches :: Behavior a -> Stream (Behavior a) -> Behavior a
fromSwitches i (Stream s) = s >>= \t -> loop (Eht i t) 
  where loop (Eht h t) =  h `switch` (loop <$> t)

(<@@>) :: Behavior (a -> b) -> Stream a -> Stream b
b <@@> (Stream s) = Stream (s >>= \e -> plan (loop <$> e))  where
  loop (Eht h t) = do h' <- b <*> pure h
                      t' <- plan (loop <$> t)
                      return (Eht h' t')


scanlEvPrivate :: (a -> b -> a) -> a -> Eht b -> Eht a
scanlEvPrivate f i (Eht h t) = 
   let i' = f i h
   in Eht i' (scanlEvPrivate f i' <$> t)

scanlEv :: (a -> b -> a) -> a -> Stream b -> Behavior (Stream a)
scanlEv f i (Stream es) = 
   do e <- es
      let s = scanlEvPrivate f i <$> e
      return (Stream (wrap s))

catMaybesEv :: Stream (Maybe a) -> Stream a
catMaybesEv (Stream s) = Stream $
  do e <- s
     wrap (e >>= catM) where
  catM (Eht Nothing  t) = t >>= catM
  catM (Eht (Just x) t) = pure (Eht x (t >>= catM))

merge :: Stream a -> Stream a -> Stream a
merge (Stream l) (Stream r) = Stream $
  do el <- l
     er <- r
     mergem el er where

mergem :: Es a -> Es a -> Behavior (Es a)
mergem el er = 
    do ev <- first (Left <$> el) (Right <$> er)
       plan (mergeHT <$> ev)  where
    mergeHT (Left  (Eht h t)) = Eht h <$> mergem t er
    mergeHT (Right (Eht h t)) = Eht h <$> mergem el t



filterEv :: Stream Bool -> Stream ()
filterEv es = catMaybesEv (toJust <$> es)
  where toJust True = Just ()
        toJust False = Nothing


filterMapB :: Behavior (a -> Maybe b) -> Stream a -> Stream b
filterMapB f e = catMaybesEv $ f <@@> e

filterB :: Behavior (a -> Bool) -> Stream a -> Stream a
filterB f = filterMapB (toMaybe <$> f) 
  where toMaybe f = \a ->  if f a then Just a else Nothing

during :: Stream a -> Behavior Bool -> Stream a
e `during` b = filterB (const <$> b) e

sampleOn :: Behavior a -> Stream x -> Stream a
sampleOn b s = (const <$> b) <@@> s

bufferStream :: Int -> Stream a -> Behavior (Stream [a])
bufferStream i = scanlEv (\t h -> take i (h : t)) []

next :: Stream a -> Behavior (Event a)
next (Stream s) = (getHead <$>) <$> s where
  getHead (Eht h _) = h

nextAll :: Stream a -> Behavior (Event [a])
nextAll (Stream s) = do e <- s
                        plan (getAll <$> e) where
  getAll (Eht h t) = occ t >>= \case
                       Just n -> (h :) <$> getAll n
                       Nothing -> return [h]

foldB :: Behavior a -> (Behavior a -> b -> Behavior a) -> Stream b -> Behavior (Behavior a)
foldB b f es = fromSwitches b <$> scanlEv f b es

fold :: (a -> b -> a) -> a -> Stream b -> Behavior (Behavior a)
fold f i = foldB (pure i) f' 
  where f' b x = (\b -> f b x) <$> b

parList :: Stream (Mortal b ()) -> Behavior (Behavior [b])
parList = foldB (pure []) (flip (.:)) 



-- See reflection without remorse for which performance problem this construction solves...

type EvsEnd     x = Seq (EvsEndView x)
type EvsEndView x = Event (EH x)
data EH x = x :| EvsEnd x | End

fromView :: EvsEndView x -> EvsEnd x
fromView e = singleton e

toView :: EvsEnd x -> EvsEndView x
toView e = case viewl e of
     EmptyL -> return End
     h :< t -> h >>= nxt t
  where nxt t End = toView t
        nxt r (h :| l) = let q = l >< r in return (h :| q)

append :: EvsEnd x -> EvsEnd x -> EvsEnd x
append = (><)

app :: EvsEnd x -> Event (EvsEnd x) -> EvsEnd x
app l r = append l (fromView $ r >>= toView) -- it's magic!

emptyEmits = empty
singleEmit x = singleton (return (x :| emptyEmits))

toEventStream :: EvsEnd x -> Stream x
toEventStream e = Stream $ wrap (toView e >>= convert) where
  convert End      = never
  convert (x :| t) = pure (Eht x (toView t >>= convert))


data StreamEnd x a = StreamEnd { emits :: EvsEnd x, eend :: Event a }

instance Monad (StreamEnd x) where
  return x = StreamEnd emptyEmits (return x)
  (StreamEnd s e) >>= f = let fv = fmap f e
                              fs = emits <$> fv
                              fa = fv >>= eend
                          in StreamEnd (app s fs) fa


instance (Monad b, Swap b Event) => Swap b (StreamEnd x) where
  swap (StreamEnd b e) = liftM (StreamEnd b) (plan e)


emit :: (Monad b, Swap b (StreamEnd x)) => x -> (b :. StreamEnd x) ()
emit x = liftRight $ StreamEnd (singleEmit x) (return ())

waitFor :: (Monad b, Swap b (StreamEnd x)) =>
          b (Event a) -> (b :. StreamEnd x) a
waitFor e =  liftLeft e >>= liftRight . (StreamEnd emptyEmits)

instance Functor (StreamEnd x) where fmap = liftM
instance Applicative (StreamEnd x) where pure = return ; (<*>) = ap

runEventStreamM :: Monad b => (b :. StreamEnd x) a -> b (Stream x, Event a)
runEventStreamM s = do StreamEnd s e <- (open s)
                       return (toEventStream s, e)
  

printAll :: Show a => Stream a -> Now ()
printAll s = again where
  again =
   do e <- cur (nextAll s)
      plan (loop <$> e)
      return () where
  loop l = do unsafeSyncIO (mapM_ (putStrLn . show) l)
              again

