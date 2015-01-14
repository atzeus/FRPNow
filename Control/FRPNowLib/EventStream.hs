{-# LANGUAGE DeriveFunctor,FlexibleInstances,ConstraintKinds,ViewPatterns,NoMonomorphismRestriction,MultiParamTypeClasses ,FlexibleContexts,TypeOperators, LambdaCase, ScopedTypeVariables, Rank2Types, GADTs, TupleSections,GeneralizedNewtypeDeriving, UndecidableInstances #-}

module Control.FRPNowLib.EventStream
(EventStream(..), next, nextSim, emptyEs, repeatEv, merge, switchEs, singletonEs, fmapB, filterJusts, foldB, fold, during, sampleOn, parList, scanlEv, last,filterEv, bufferStream, foldr1Ev, foldrEv, foldrSwitch, changes, EventStreamM, emit,runEventStreamM, printAll, repeatEvN)
  where

import Control.FRPNowImpl.FRPNow
import Control.FRPNowLib.Lib
import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative hiding (empty)
import Control.Monad.Swap
import Data.Sequence hiding (reverse,scanl,take)
import Prelude hiding (until,length)
import Debug.Trace

{-
type EVS a = Event (EHT a)
data EHT a = a :|| EVS a

fmapBe :: Behavior (a -> b) -> EVS a -> Behavior (EVS b)
fmapBe b e =  plan (apply <$> e) where
  apply (h :|| t) = do h' <- b <*> pure h
                       t' <- fmapBe b e
                       return (h' :|| t')

filterJustse :: EVS (Maybe a) -> EVS a
filterJustse e = 
  do h :|| t <- e
     let t' = filterJustse t
     case h of
      Just x  -> return (x :|| t')
      Nothing -> t'

scanlEve :: (a -> b -> a) -> a -> EVS b -> EVS a
scanlEve f i e = loop i <$> e where
  loop i (h :|| t) = let i' = f i h
                     in i' :|| (loop i' <$> t)

foldr1Eve :: (a -> Event b -> b) -> EVS a -> Event b
foldr1Eve f e = loop <$> e where
  loop (h :|| t) = let t' = loop <$> t
                   in f h t'

wrap :: EVS a -> Behavior (EVS a)
wrap e = getNow e >>= \case
            Just (h :|| t) -> wrap t
            Nothing -> pure e `switch` (wrapht <$> e)
 where wrapht (_ :|| t) = wrap t

-}

newtype EventStream a = Es { getEs :: Behavior (Event [a]) }

instance Functor EventStream where
  fmap f (Es b) = Es $ (fmap f <$>) <$> b

next :: EventStream a -> Behavior (Event a)
next e = fmap head <$> getEs e

nextSim :: EventStream a -> Behavior (Event [a]) 
nextSim e = getEs e

repeatEvN :: Now (Event [a]) -> Now (EventStream a)
repeatEvN en = Es <$> loop where
  loop = do e <-  en
            e' <- planIO (loop <$ e)
            return (pure e `switch` e')



repeatEv :: Behavior (Event a) -> EventStream a
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

tailEs :: EventStream a -> Behavior (Event (EventStream a))
tailEs es = fmap (const es) <$> getEs es
               
switchEs :: EventStream a -> EventStream a -> EventStream a
switchEs l r = Es $ do r' <- fmap getEs <$> tailEs r
                       getEs (merge l r) `switch` r'

emptyEs :: EventStream a
emptyEs = Es $ return never

singletonEs :: Event a -> EventStream a
singletonEs e = Es $ pure (fmap (\x -> [x]) e) `switch` fmap (const (getEs emptyEs)) e



fmapB :: Behavior (a -> b) -> EventStream a -> EventStream b
fmapB f es = Es $ loop where
 loop =  
   do e  <- getEs es
      e' <- plan (fmap nxt e)
      pure e' `switch` (loop <$ e')
 nxt l = do fv <- f ; return (fmap fv l)


nextJusts :: EventStream (Maybe a) -> Behavior (Event [a])
nextJusts es = loop where
  loop = 
    do e <- getEs es
       ev <- join <$> plan (fmap nxt e)
       return ev
  nxt l = case catMaybes l of
              [] -> loop
              l  -> return (return l)

filterJusts :: EventStream (Maybe a) -> EventStream a
filterJusts es = Es loop where
  loop =  do e <- nextJusts es
             pure e `switch` (loop <$ e)

filterEv :: EventStream Bool -> EventStream ()
filterEv es = filterJusts (toJust <$> es)
  where toJust True = Just ()
        toJust False = Nothing


filterMapB :: Behavior (a -> Maybe b) -> EventStream a -> EventStream b
filterMapB f e = filterJusts $ fmapB f e

filterB :: Behavior (a -> Bool) -> EventStream a -> EventStream a
filterB f = filterMapB (toMaybe <$> f) 
  where toMaybe f = \a ->  if f a then Just a else Nothing

during :: EventStream a -> Behavior Bool -> EventStream a
e `during` b = filterB (const <$> b) e

sampleOn :: Behavior a -> EventStream x -> EventStream a
sampleOn b = fmapB (const <$> b) 


scanlEv :: (a -> b -> a) -> a -> EventStream b -> Behavior (EventStream a)
scanlEv f i es = Es <$> loop i where
 loop i = 
  do e  <- getEs es
     let e' = (\(h : t) -> tail $ scanl f i (h : t)) <$> e
     ev <- plan (loop . last <$> e')
     return (pure e' `switch` ev)

foldr1Ev :: (a -> Event b -> b) -> EventStream a -> Behavior (Event b)
foldr1Ev f es = loop where
 loop = 
  do e  <- getEs es
     ev <- plan (nxt <$> e)
     pure ev
 nxt [h]     = f h          <$> loop
 nxt (h : t) = f h . return <$> nxt t

foldrEv :: a -> (a -> Event b -> b) -> EventStream a -> Behavior b
foldrEv i f es = f i <$> foldr1Ev f es

foldrSwitch :: Behavior a -> EventStream (Behavior a) -> Behavior (Behavior a)
foldrSwitch b = foldrEv b switch

foldB :: Behavior a -> (Behavior a -> b -> Behavior a) -> EventStream b -> Behavior (Behavior a)
foldB b f es = scanlEv f b es >>= foldrSwitch b

fold :: (a -> b -> a) -> a -> EventStream b -> Behavior (Behavior a)
fold f i = foldB (pure i) f' 
  where f' b x = (\b -> f b x) <$> b

parList :: EventStream (BehaviorEnd b ()) -> Behavior (Behavior [b])
parList = foldB (pure []) (flip (.:)) 

bufferStream :: Int -> EventStream a -> Behavior (EventStream [a])
bufferStream i = scanlEv (\t h -> take i (h : t)) []

changes :: Eq a => Behavior a -> EventStream a
changes = repeatEv . change 



-- See reflection without remorse for which performance problem this construction solves...

type Evs     x = Seq (EvsView x)
type EvsView x = Event (EH x)
data EH x = x :| Evs x | End


toView :: Evs x -> EvsView x
toView e = case viewl e of
     EmptyL -> return End
     h :< t -> h >>= nxt t
  where nxt t End = toView t
        nxt r (h :| l) = let q = l >< r in return (h :| q)

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
  nxt :: [x] -> EH x -> Behavior (Event ([x]))
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

emit :: (Swap (BehaviorEnd x) f, Monad f) =>  x -> (f :. EventStreamM x) ()
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
      do syncIO (mapM_ (putStrLn . show) l)
         e2 <- cur (nextSim evs)
         plan (loop <$> e2)
         return ()            









