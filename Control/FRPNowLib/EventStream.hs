{-# LANGUAGE FlexibleInstances,ConstraintKinds,ViewPatterns,NoMonomorphismRestriction,MultiParamTypeClasses ,FlexibleContexts,TypeOperators, LambdaCase, ScopedTypeVariables, Rank2Types, GADTs, TupleSections,GeneralizedNewtypeDeriving #-}

module Control.FRPNowLib.EventStream
-- (EventStream, next, nextSim, once, silent ,fmapB, merge, filterJusts, foldB, fold, filterB, filterE, parList, EventStreamEnd(..), emit)
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


newtype EventStream a = Es { getEs :: Behaviour (Event [a]) }



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

csingleton :: Event [a] -> EventStream a
csingleton e = Es $ let e' = e >>= \case 
                                [] -> never
                                l  -> return l
                    in pure e' `switch` fmap (const (getEs emptyEs)) e'

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


-- Event (EventStream x) cannot emit when the event occurs!
flatEventStream :: Event ([x], EventStream x) -> EventStream x
flatEventStream e = csingleton (fmap fst e) `switchEs` flatEs (fmap snd e)
 where flatEs es = Es $ join <$> plan (fmap getEs es)


newtype EventStreamM x a = EM { emits :: [x] , rest :: Behaviour (Either (Event [x]) a) }





