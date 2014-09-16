{-# LANGUAGE GADTs, TupleSections, ScopedTypeVariables #-}

module Lib where

import IO.Implementation 
import Race
import Control.Applicative
import Control.Monad hiding (when)
import TermM

cur :: Event s a -> Behaviour s (Maybe a)
cur e = pure Nothing `switch` fmap (pure . Just) e

joinEB :: Behaviour s (Event s (Behaviour s a)) -> Behaviour s (Event s a)
joinEB b = whenJust $ 
  do e <- b
     v <- cur e
     case v of
       Just b2 -> do x <- b2; return (Just x)
       Nothing -> return Nothing

race :: Event s a -> Event s b -> Behaviour s (Event s (Race a b))
race a b = whenJust $ combineMaybe <$> cur a <*> cur b

when :: (a -> Bool) -> Behaviour s a -> Behaviour s (Event s a)
when f b = whenJust $ choose <$> b where
  choose x | f x = Just x
           | otherwise = Nothing
--- Behaviours with an endpoint ---

data BehaviourEnd s x a = BE { behaviour :: Behaviour s x, endEv :: Event s a }

until :: Behaviour s x -> Event s a -> BehaviourEnd s x a
until = BE

instance Monad (BehaviourEnd s x) where
  return x = BE (pure undefined) (pure x)
  (BE b e) >>= f = 
    let v = fmap f e
        vb = fmap behaviour v
        ve = join $ fmap endEv v
    in BE (b `switch` vb) ve


zipBE :: (a -> b -> b) -> BehaviourEnd s a x -> Behaviour s b -> Behaviour s b
zipBE f (BE bx e) b = f <$> bx <*> b `switch` fmap (const b) e

(.:) :: BehaviourEnd s a x -> Behaviour s [a] -> Behaviour s [a]
(.:) = zipBE (:)

-- event streams 

newtype EventStream s a = ES (Behaviour s (Event s [a]))
-- this is a bit tricky
--
-- always points to the next event of
-- of the results of all simultanious events
-- then switches to next simultanious events
-- hence is isomorphic to:
-- type EventStream s a = Event s (EHT s a)
-- data EHT a = a :| EventStream s a
-- but this does not have space leak...

nextES :: EventStream s a -> Behaviour s (Event s a)
nextES (ES a) = fmap last <$> a

changes :: Eq a => Behaviour s a -> EventStream s a
changes b = ES loop where
  loop = do v <- b
            e <- when (/= v) b
            pure (fmap (\x -> [x]) e) `switch` fmap (const loop) e

emptyES :: EventStream s a
emptyES = ES (pure never)                                                

singletonES :: Event s a -> EventStream s a
singletonES a = onceES $ fmap (\x -> [x]) a

onceES :: Event s [a] -> EventStream s a
onceES a = ES $ pure a

foldES :: (a -> b -> a) -> a -> EventStream s b -> Behaviour s a
foldES f i (ES es) = do e <- es 
                        pure i `switch` fmap (loop i) e
  where loop i [] = do e <- es
                       pure i `switch` fmap (loop i) e
        loop i (h : t) = loop (f i h) t

mergeEventStreams :: EventStream s a -> EventStream s a -> EventStream s a
mergeEventStreams (ES l) (ES r) = ES loop where
 loop = 
   do lh <- l
      rh <- r
      r <- race lh rh
      pure (fmap raceToList r) `switch` fmap (const loop) r

raceToList :: Race [a] [a] -> [a]
raceToList (L a)     = a
raceToList (R b)     = b
raceToList (Tie a b) = a ++ b

instance Functor (EventStream s) where
  fmap f (ES b) = ES $ fmap (fmap (fmap f)) b 

filterES :: forall a s. (a -> Bool) -> EventStream s a -> EventStream s a
filterES f (ES es) = ES loop where
  loop = do r <- getNext
            pure r `switch` fmap (const loop) r
  getNext :: Behaviour s (Event s [a])
  getNext = do e1 <- es
               fmap join $ joinEB $ pure $ fmap choose e1
 
  choose :: [a] -> Behaviour s (Event s [a])
  choose x = case filter f x of
                [] -> getNext
                r  -> return (pure r)



switchES :: forall a s. EventStream s a -> Event s (EventStream s a) -> EventStream s a
switchES (ES es) ev = ES loop where
 loop :: Behaviour s (Event s [a])
 loop = do x <- es
           r <- race x ev           
           let first = fmap join $ joinEB $ pure $ fmap raceResult r
           first `switch` fmap next r
 next (L _)     = loop
 next (Tie _ (ES b)) = b
 next (R (ES b))     = b

 raceResult :: Race [a] (EventStream s a) -> Behaviour s (Event s [a])
 raceResult (L a)          = pure $ pure a
 raceResult (R (ES b))     = b
 raceResult (Tie a (ES b)) = 
    do e <- b
       ev <- cur e
       case ev of
         Just bl -> return $ pure (a ++ bl)
         Nothing -> return $ pure a


data EventStreamM s x a = ESM { stream :: EventStream s x, end :: Event s a }

instance Monad (EventStreamM s x) where
  return x = ESM emptyES (pure x)
  -- this bind has a MAJOR problem because of possible multiple instances of switchES, use codensityT, see reflection without remorse
  (ESM s e) >>= f = 
    let fv = fmap f e
        fes = fmap stream fv
        fee = join $ fmap end fv
    in ESM (switchES s fes) fee

yield :: x -> EventStreamM s x ()
yield a = ESM (singletonES (pure a)) (pure ())

waitFor :: Event s a -> EventStreamM s x a
waitFor e = ESM emptyES e
                     

parList :: EventStream s (BehaviourEnd s a ()) -> Behaviour s [a]
parList = join . foldES (flip (.:)) (pure [])



