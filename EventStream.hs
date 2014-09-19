{-# LANGUAGE ScopedTypeVariables, GADTs, TupleSections,GeneralizedNewtypeDeriving #-}

module EventStream(
      EventStream, repeatEv, nextES, nextESSimul, foldES, mapES, filterES, on, appOn, parList, foldESB, foldESp,
      EventStreamM, emit, waitEv, waitJust, wait, waitB, waitIO, liftES, runEventStreamM,
      Void, EventM,runEventM, 
      becomesJust,becomesTrue) where

import Implementation
import Lib
import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative
import Util.TermM

repeatEv :: Behaviour s (Event s a) -> Behaviour s (EventStream s a)
repeatEv b = fst <$> runEventStreamM loop where
 loop = do ev <- liftB b
           x <- waitEv ev
           emit x
           loop

becomesTrue :: Behaviour s Bool -> Behaviour s (Event s ())
becomesTrue b = becomesJust $ boolToJust <$> b
  where boolToJust True = Just ()
        boolToJust False = Nothing

becomesJust :: Behaviour s (Maybe a) -> Behaviour s (Event s a)
becomesJust b = runEventM $  
   do bv <- liftB b
      case bv of
        Just _  -> wait (not . isJust <$> b)
        Nothing -> return ()
      waitJust b 


newtype EventStream s a = ES { unwrap :: Behaviour s (EventStreamImpl s a) }

wrapB :: EventStreamImpl s a ->  Behaviour s (EventStreamImpl s a)  
wrapB =  loop where
  loop e = pure e `switch` fmap (loop . nxt) e -- forget past!
  nxt (_ :< t) = t
  

nextES :: EventStream s a -> Behaviour s (Event s a)
nextES e = fmap last <$> nextESSimul e

nextESSimul :: EventStream s a -> Behaviour s (Event s [a])
nextESSimul es =  do e <- unwrap es
                     e' <- plan $ fmap (getAll []) e
                     return (fmap reverse e')
  where getAll l (h :< t) = do tc <- cur t
                               case tc of
                                 Just x -> getAll (h : l) x
                                 Nothing -> return (h : l)

foldESp :: (a -> b -> a) -> a -> EventStream s b -> Behaviour s (Behaviour s a)
foldESp f = foldES (\x y -> return $ f x y)

parList :: EventStream s (Behaviour s (BehaviourEnd s a x)) -> Behaviour s (Behaviour s [a])
parList = foldESB startPar (pure []) where
  startPar t h = (.: t) <$> h


foldES :: (a -> b -> Behaviour s a) -> a -> EventStream s b -> Behaviour s (Behaviour s a)
foldES f i es = do e <- unwrap es
                   foldESImpl f i e

foldESB :: forall a b s. (Behaviour s a -> b -> Behaviour s (Behaviour s a)) -> Behaviour s a -> EventStream s b -> Behaviour s (Behaviour s a)
foldESB f i es = do e <- unwrap es
                    foldESBImpl f i e


mapES :: (a -> b) -> EventStream s a -> EventStream s b
mapES f es = ES $
   do e <- unwrap es
      wrapB (mapESImpl f e)

filterES :: (a -> Bool) -> EventStream s a -> EventStream s a
filterES f es = ES $
   do e <- unwrap es
      e' <- filterESImpl f e
      wrapB e'

on :: Behaviour s a -> EventStream s x -> EventStream s a
on b = appOn (const <$> b)

appOn :: Behaviour s (a -> b) -> EventStream s a -> EventStream s b
appOn b es =  ES $
   do e <- unwrap es
      e' <- appOnImpl b e
      wrapB e'

newtype EventStreamM m x s a = EM (EventStreamMImpl m x s a) deriving (Functor, Applicative, Monad)
                 

emit :: x -> EventStreamM m x s ()
emit x = EM (prim (Yield x))

liftES :: m s a -> EventStreamM m x s a
liftES b =  EM (prim (Lift b))

waitEv :: Event s a -> EventStreamM m x s a
waitEv e = EM (prim (Wait e))

waitJust :: BehaviourLike m => Behaviour s (Maybe a) -> EventStreamM m x s a
waitJust b = do e <- liftB $ whenJust b
                waitEv e

wait :: BehaviourLike m => Behaviour s Bool -> EventStreamM m x s ()
wait b =  do e <- liftB $ when b
             waitEv e

waitB b = do ev <- liftB b
             waitEv ev

waitIO :: IO a -> EventStreamM Now x s a
waitIO i = do e <- liftES $ act i
              waitEv e

instance BehaviourLike m => BehaviourLike (EventStreamM m x) where
  liftB = liftES . liftB
                   

type EventStreamImpl s a = Event s (EVS s a)
data EVS s a = a :< EventStreamImpl s a

mapESImpl :: (a -> b) -> EventStreamImpl s a -> EventStreamImpl s b
mapESImpl f = fmap loop where
  loop (h :< t)  = f h :< fmap loop t

foldESBImpl :: forall a b s. (Behaviour s a -> b -> Behaviour s (Behaviour s a)) -> Behaviour s a -> EventStreamImpl s b -> Behaviour s (Behaviour s a)
foldESBImpl f i e = do s <- plan $ fmap (nxt i) e
                       return (i `switch` s) where
    nxt i (h :< t) = do fv <- f i h
                        t' <- plan $ fmap (nxt fv) t
                        return (fv `switch` t')


foldESImpl :: forall a b s. (a -> b -> Behaviour s a) -> a -> EventStreamImpl s b -> Behaviour s (Behaviour s a)
foldESImpl f i e = do s <- plan $ fmap (nxt i) e
                      return (pure i `switch` s) where
    nxt i (h :< t) = do fv <- f i h
                        t' <- plan $ fmap (nxt fv) t
                        return (pure fv `switch` t')


filterESImpl :: forall s a. (a -> Bool) -> EventStreamImpl s a -> Behaviour s (EventStreamImpl s a)
filterESImpl f e = join <$> (plan $ fmap loop e) where
  loop :: EVS s a -> Behaviour s (EventStreamImpl s a)
  loop (h :< t) = nxt <$> join <$> (plan $ fmap loop t)
    where nxt t' | f h       = pure (h :< t')
                 | otherwise = t'


appOnImpl :: Behaviour s (a -> b) -> EventStreamImpl s a -> Behaviour s (EventStreamImpl s b)
appOnImpl b es =  plan $ fmap nxt es
  where nxt (h :< t) = do h' <- b <*> pure h
                          t' <- plan $ fmap nxt t
                          return (h' :< t')
                        


data Void 

type EventM m s a = EventStreamM m Void s a


runEventM ::(BehaviourLike m, PlanMonad m, Monad (m s)) =>
             EventM m s a -> m s (Event s a)
runEventM e = runEventStreamM e >>= \(_,a) -> return a


type EventStreamEnd s x a = Event s (EVSE s x a)
      
data EVSE s x a = x :| EventStreamEnd s x a
                | End a



data EVSPrim m x s a where
  Yield :: x -> EVSPrim m x s ()
  Lift  :: m s a -> EVSPrim m x s a
  Wait  :: Event s a -> EVSPrim m x s a


-- use term monad to omit reflection without remorse problem...
type EventStreamMImpl m x s a = TermM (EVSPrim m x s) a


runEventStreamM :: (BehaviourLike m, PlanMonad m, Monad (m s)) =>
                   EventStreamM m x s a -> m s (EventStream s x, Event s a)
runEventStreamM (EM t) = 
   do ese <- runEventStreamM' t
      let es = getEventStream ese
      let ee = getEnd ese 
      return (ES $ wrapB es, ee)
                       

getEventStream :: EventStreamEnd s x a -> EventStreamImpl s x 
getEventStream e = e >>= loop where
  loop (a :| t) = pure (a :< (t >>= loop))
  loop (End _)  = never

getEnd :: EventStreamEnd s x a -> Event s a
getEnd e = e >>= loop where
  loop (_ :| t) = t >>= loop
  loop (End a)  = return a

runEventStreamM' ::  forall m s x a b.  (BehaviourLike m, PlanMonad m, Monad (m s)) =>
                   EventStreamMImpl m x s a -> m s (EventStreamEnd s x a)
runEventStreamM' t = case viewTermM t of
  Return a -> return $ pure $ End a
  p :>>= f -> do x <- handlePrim p; bindES x (runEventStreamM' . f)
 where handlePrim :: EVSPrim m x s v -> m s (EventStreamEnd s x v)
       handlePrim (Yield a)  = return $ pure $ a :| pure (End ())
       handlePrim (Wait e)   = return $ fmap End e
       handlePrim (Lift b)  = b >>= \x -> return $ pure (End x)

bindES ::  forall m s x a b.  (BehaviourLike m, PlanMonad m, Monad (m s)) =>
           EventStreamEnd s x a -> (a -> m s (EventStreamEnd s x b)) -> m s (EventStreamEnd s x b)
bindES e f = do e' <- plan $ fmap nxt e
                return $  join e'
   where nxt :: EVSE s x a -> m s (Event s (EVSE s x b))
         nxt (x :| t) = do t' <- plan $ fmap nxt t
                           return $ return $ x :| (join t')
         nxt (End a)  = f a 

