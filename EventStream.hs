{-# LANGUAGE LambdaCase, ScopedTypeVariables, Rank2Types, GADTs, TupleSections,GeneralizedNewtypeDeriving #-}

module EventStream
 (EventStream, next, nextSim, (<@@>),filterJusts,filterE, silent,merge, foldB, EventM, emit, waitFor,runEventM)
  where

import FRPNow
import Lib
import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative
import Data.Monoid


newtype EventStream a = WrapES { unwrapES :: Behaviour (ES a) }

type ES a = Event (ESH a)
data ESH a = a :< ES a

instance Functor EventStream where
  fmap f b = pure f <@@> b

next :: EventStream a -> Behaviour (Event a)
next e = fmap head <$> nextSim e

-- gets all next simultanious values
nextSim :: EventStream a -> Behaviour (Event [a])
nextSim es =  do e <- fmap (getAll []) <$> unwrapES es
                 e' <- plan $ e
                 return (fmap reverse e')
  where getAll :: [a] -> ESH a -> Behaviour [a]
        getAll l (h :< t) = do tc <- getNow t
                               case tc of
                                 Just x -> getAll (h : l) x
                                 Nothing -> return (h : l)


(<@@>) :: forall a b. Behaviour (a -> b) -> EventStream a -> EventStream b
(<@@>) f b  = WrapES $ (fmap loop <$> unwrapES b) >>= plan where
  loop :: ESH a -> Behaviour (ESH b)
  loop (h :< t) = do v  <- f
                     t' <- plan $ fmap loop t
                     return (v h :< t')

filterJusts :: EventStream (Maybe a) -> EventStream a
filterJusts b = WrapES $ (>>= loop) <$> unwrapES b where
  loop :: ESH (Maybe a) -> ES a
  loop (h :< t) = 
    let t' = t >>= loop
    in case h of
       Just x  -> return $ x :< t'
       Nothing -> t'
                   
filterE :: (a -> Bool) -> EventStream a -> EventStream a
filterE f b = filterJusts $ pure toMaybe <@@> b
  where toMaybe x = if f x then Just x else Nothing

once :: x -> EventStream x
once x = WrapES $ pure $ pure (x :< never)

silent :: EventStream a
silent = WrapES $ pure never

-- in case of simultanuaty, the left elements come first
merge :: forall a. EventStream a -> EventStream a -> EventStream a
merge l r = WrapES $ 
  do lv <- unwrapES l 
     rv <- unwrapES r
     loop lv rv where
  loop :: ES a -> ES a -> Behaviour (ES a)
  loop l r = do v <- fmap next <$> firstObs r l  
                plan v where
    next (Right (lh :< lt)) = (lh :<) <$> loop lt r
    next (Left  (rh :< rt)) = (rh :<) <$> loop l  rt



foldB :: (Behaviour a -> b -> Behaviour2 a) -> Behaviour a -> EventStream b -> Behaviour2 a
foldB f i e = do e' <- unwrapES e
                 s <- plan $ fmap (nxt i) e'
                 return (i `switch` s) where
    nxt i (h :< t) = do fv <- f i h
                        t' <- plan $ fmap (nxt fv) t
                        return (fv `switch` t')
{-
parList :: EventStream s (Behaviour s (BehaviourEnd s a x)) -> Behaviour s (Behaviour s [a])
parList = foldESB startPar (pure []) where
  startPar t h = (.: t) <$> h
-}
instance Monoid (EventStream a) where
  mempty = silent
  mappend = merge

switchES :: forall a. EventStream a -> Event (EventStream a) -> EventStream a
switchES e s = WrapES $ 
   do let us = fmap unwrapES s
      s' <- join <$> plan us
      e' <- unwrapES e
      esSwitch e' s' `switch` us where
  esSwitch :: ES a -> ES a -> Behaviour (ES a)
  esSwitch l r = do v <- fmap next <$> firstObs l r  
                    plan v where
    next :: Either (ESH a) (ESH a) -> Behaviour (ESH a)
    next (Right r)          = pure r
    next (Left  (lh :< lt)) = (lh :<) <$> esSwitch lt  r

-- Todo: avoid performance problem see paper "Reflection without Remorse" (plug, mine)
data EventStreamEnd x a = EVE { stream :: EventStream x, end :: Event a }

instance Monad (EventStreamEnd x) where
   return x = EVE silent (pure x)
   (EVE s e) >>= f = let fv = fmap f e
                         vs = fmap stream fv 
                         es = fv >>= end
                     in EVE (s `switchES` vs)  es


emitESE :: x -> EventStreamEnd x ()
emitESE x = EVE (once x) (return ())

waitForESE :: Event a -> EventStreamEnd x a
waitForESE = EVE silent 

instance EventLike (EventStreamEnd x) where
  plan (EVE s e) = EVE s <$> plan e

type EventM x a = BehaviourTrans (EventStreamEnd x) a

emit :: x -> EventM x ()
emit = lift . emitESE

waitFor :: Behaviour (Event a) -> EventM x a
waitFor b = cur b >>= lift  . waitForESE

runEventM :: EventM x a -> Behaviour (EventStream x, Event a)
runEventM e = do v <- runBT e
                 return (stream v, end v)




{-




data Void 
newtype EventStreamEnd x a = EventStreamEnd { getESE :: ESE x a }
type ESE  x a = Event (EVSE x a)
data EVSE x a = x :| ESE x a
              | End a

instance Monad (EventStreamEnd x) where
  return = EventStreamEnd . pure . End 
  e >>= f = EventStreamEnd $ (getESE e) >>= loop
   where loop (h :| t) = return (h :| (t >>= loop))
         loop (End x)  = getESE (f x)




newtype CodensityT m a = Cod { getCod :: forall b. (a -> m b) -> m b }

abs :: Monad m => CodensityT m a -> m a
abs (Cod a) = a return
rep :: Monad m => m a -> CodensityT m a
rep m = Cod (m >>=)

instance Monad m => Monad (CodensityT m) where
  return a = rep ( return a)
  (Cod m) >>= f = Cod $ m . (flip (getCod . f))
-}
{-
foldBI :: Behaviour (a -> b -> a) -> Behaviour a -> EVS b -> Behaviour (EVS b)
foldBI f (h :< t) = 
-}
{-
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
  



foldESp :: (a -> b -> a) -> a -> EventStream s b -> Behaviour s (Behaviour s a)
foldESp f = foldES (\x y -> return $ f x y)




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
                   

                        




type EventM m s a = EventStreamM m Void s a


runEventM ::(BehaviourLike m, PlanMonad m, Monad (m s)) =>
             EventM m s a -> m s (Event s a)
runEventM e = runEventStreamM e >>= \(_,a) -> return a



      
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


-}

