{-# LANGUAGE GADTs, TupleSections, ScopedTypeVariables,ConstraintKinds,FlexibleContexts,UndecidableInstances #-}

module Lib where

import IO.Implementation 
import Race
import Control.Applicative
import Control.Monad hiding (when)
import TermM
import Debug.Trace
type StartBehaviour s a = Behaviour s (Behaviour s a)

class BehaviourLike m where
  plan :: Event s (m s a) -> m s (Event s a)
  liftB :: Behaviour s a -> m s a

instance BehaviourLike Behaviour where
  plan = planBehaviour
  liftB = id
 
instance BehaviourLike Now where
  plan = planNow
  liftB = liftBehaviour

type BehaviourL m s = (Monad (m s), BehaviourLike m)

cur :: Event s a -> Behaviour s (Maybe a)
cur e = pure Nothing `switch` fmap (pure . Just) e

race :: Event s a -> Event s b -> Behaviour s (Event s (Race a b))
race a b = whenJust $ combineMaybe <$> cur a <*> cur b

when :: (a -> Bool) -> Behaviour s a -> Behaviour s (Event s a)
when f b = whenJust $ choose <$> b where
  choose x | f x = Just x
           | otherwise = Nothing


-- event streams 

type EventStream s a = Behaviour s (Event s [a])
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
nextES a = fmap last <$> a

type StartEventStream s a = Behaviour s (EventStream s a)


emptyES :: EventStream s a
emptyES = pure never                                              

singletonES :: Event s a -> EventStream s a
singletonES a = onceES $ fmap (\x -> [x]) a

onceES :: Event s [a] -> EventStream s a
onceES a =  pure a



mapES :: (a -> b) -> EventStream s a -> EventStream s b
mapES f b = fmap (map f) <$> b


type EventStreamM m s a b = m s (Event s [a], Event b)

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


type YieldM m s x a = TermM (YieldPrim m s x) a

data YieldPrim m s x a where
  Yield :: x -> YieldPrim m s x a
  WaitFor :: Event s a -> YieldPrim m s x a
  Lift  :: m s a -> UntilMPrim m s x a


runYieldM :: forall m s x a . BehaviourL m s => 
             UntilM m s x a -> m s (Behaviour s x, Event s a)
runYieldM t = handle (pure undefined) (pure ()) t

newtype EventStreamM m s a = EventM { runEventM :: m s (EventStream s x, Event s a) }
instance BehaviourL m s => Monad (EventStreamM m s) where
  return x = EventM (return $ (pure never, pure x))
  (EventM m) >>= f = EventM $ 
        do (s,e) <- m
           e2 <- plan $ fmap (runEventM . f) e
           let ee = e2 >>= snd
           let es = fmap fst ee2
           return (switchES s es, ee)




-- until monad ----

type UntilM m s x a = TermM (UntilMPrim m s x) a
-- needs deep embedding to do self
data UntilMPrim m s x a where
  Until :: Behaviour s x -> Event s a -> UntilMPrim m s x a
  Lift  :: m s a -> UntilMPrim m s x a
  Self  :: UntilMPrim m s x x

untill b e = prim (Until b e)
self  = prim Self
liftUM l = prim (Lift l)

runUntilM :: forall m s x a . BehaviourL m s => 
             UntilM m s x a -> m s (Behaviour s x, Event s a)
runUntilM t = handle (pure undefined) (pure ()) t
  where 
    handle :: Behaviour s x -> Event s () -> UntilM m s x a -> m s (Behaviour s x, Event s a)
    handle prevB prevE t = 
       case viewTermM t of 
         Return x -> return (prevB, fmap (const x) prevE)
         p :>>= f -> do (b,e) <- handlePrim p
                        let eunit = fmap (const ()) e
                        n <- plan $ fmap (handle b eunit . f) e
                        let ee = n >>= snd
                        let be = fmap fst n
                        return (b `switch` be, ee) 
     where
      handlePrim :: UntilMPrim m s x v -> m s (Behaviour s x, Event s v)
      handlePrim (Until b e) = return (b,e)
      handlePrim (Lift m)    = do mv <- m
                                  return (prevB, fmap (const mv) prevE)
      handlePrim Self        = do e' <- liftB $ plan $ fmap (const prevB) prevE
                                  return (prevB, e')
















{-
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
-}
