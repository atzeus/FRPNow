{-# LANGUAGE DeriveFunctor,FlexibleInstances,ConstraintKinds,ViewPatterns,NoMonomorphismRestriction,MultiParamTypeClasses ,FlexibleContexts,TypeOperators, LambdaCase, ScopedTypeVariables, Rank2Types, GADTs, TupleSections,GeneralizedNewtypeDeriving #-}

module Control.FRPNowLib.EventStream
 (EventStream, next, nextSim, emptyEs, repeatEv, merge, switchEs, singletonEs, fmapB, filterJusts, foldB, fold, during, sampleOn, parList,  EventStreamM, emit,runEventStreamM)
  where

import Control.FRPNowImpl.Event
import Control.FRPNowImpl.Behaviour
import Control.FRPNowLib.Lib
import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative hiding (empty)
import Data.Monoid
import Control.Monad.Swap
import Data.Sequence hiding (reverse)
import Prelude hiding (until)
import Debug.Trace


newtype EventStream a = Es { getEs :: Behaviour (Event [a]) }

next :: EventStream a -> Behaviour (Event a)
next e = fmap head <$> getEs e

nextSim :: EventStream a -> Behaviour (Event [a]) 
nextSim e = getEs e

repeatEv :: Behaviour (Event a) -> EventStream a
repeatEv b = Es $ loop where
   loop = do e <- b
             let e' = (\x -> [x]) <$> e
             pure e' `switch` (loop <$ e)

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

singletonEs :: Event a -> EventStream a
singletonEs e = Es $ pure (fmap (\x -> [x]) e) `switch` fmap (const (getEs emptyEs)) e


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

filterMapB :: Behaviour (a -> Maybe b) -> EventStream a -> EventStream b
filterMapB f e = filterJusts $ fmapB f e

filterB :: Behaviour (a -> Bool) -> EventStream a -> EventStream a
filterB f = filterMapB (toMaybe <$> f) 
  where toMaybe f = \a ->  if f a then Just a else Nothing

during :: EventStream a -> Behaviour Bool -> EventStream a
e `during` b = filterB (const <$> b) e

sampleOn :: Behaviour a -> EventStream x -> EventStream a
sampleOn b = fmapB (const <$> b) 

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

parList :: EventStream (BehaviourEnd b ()) -> Behaviour (Behaviour [b])
parList = foldB (flip (.:)) (pure [])


-- See reflection without remorse for which performance problem this construction solves...

type Evs     x = Seq (EvsView x)
type EvsView x = Event (EH x)
data EH x = x :| Evs x | End


toView :: Evs x -> EvsView x
toView e = case viewl e of
     EmptyL -> return End
     h :< t -> h >>= nxt t
  where nxt t End = toView t
        nxt r (h :| l) = return (h :| (l >< r))

append :: Evs x -> Evs x -> Evs x
append = (><)

app :: Evs x -> Event (Evs x) -> Evs x
app l r = append l (singleton $ join $ fmap toView r) -- it's magic!

emptyEmits = empty
singleEmit x = singleton (return (x :| emptyEmits))

toEventStream :: Evs x -> EventStream x
toEventStream = Es . loop where
  loop e = do e' <- lose e
              eh <- join <$> plan (nxt [] <$> toView e')
              pure eh `switch` (loop e' <$ eh)
  lose e = getNow (toView e) >>= \case 
            Just End      -> return emptyEmits
            Just (h :| t) -> lose t
            Nothing       -> return e
  nxt :: [x] -> EH x -> Behaviour (Event ([x]))
  nxt [] End     = return never
  nxt l  End     = return (return (reverse l))
  nxt l (h :| t) = getNow (toView t) >>= \case 
                    Just x -> nxt (h : l) x
                    Nothing -> return (return (reverse (h: l)))

data EventStreamM x a = EventStreamM { emits :: Evs x, eend :: Event a }

instance Monad (EventStreamM x) where
  return x = EventStreamM emptyEmits (return x)
  (EventStreamM s e) >>= f = let fv = fmap f e
                                 fs = emits <$> fv
                                 fa = fv >>= eend
                             in EventStreamM (app s fs) fa

emit :: (Swap (BehaviourEnd x) f, Monad f) =>  x -> (f :. EventStreamM x) ()
emit x = liftRight $ EventStreamM (singleEmit x) (return ())

instance Wait (EventStreamM x) where 
  waitEv = EventStreamM emptyEmits

instance (Monad b, Swap Event b) => Swap (EventStreamM x) b where
  swap (EventStreamM b e) = liftM (EventStreamM b) (plan e)

instance Functor (EventStreamM x) where fmap = liftM
instance Applicative (EventStreamM x) where pure = return ; (<*>) = ap

runEventStreamM :: EventStreamM x a -> (EventStream x, Event a)
runEventStreamM (EventStreamM s e) = (toEventStream s, e)
  




printAll :: (Show a, Eq a) => EventStream a -> Now ()
printAll evs = do e2 <- cur (nextSim evs)
                  plan (fmap loop e2)
                  return () where
  loop l = 
      do async (putStrLn (show l)) >> return ()
         e2 <- cur (nextSim evs)
         plan (fmap loop e2)
         return ()            









