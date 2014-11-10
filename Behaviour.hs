{-# LANGUAGE  DeriveFunctor, GeneralizedNewtypeDeriving #-}


module Behaviour where

import Time
import Event
import System.IO.Unsafe
import Data.IORef
import Control.Monad
import Control.Applicative
import Control.Monad.Fix

data BehaviourS a = (:->) { headB :: a, tailB ::  Event (BehaviourS a) }

instance Monad BehaviourS where
  return x        = x :-> never
  (h :-> t) >>= f = f h `switchS` fmap (>>= f) t

instance MonadFix BehaviourS where
  mfix f = fix (f . headB)

switchS :: BehaviourS a -> Event (BehaviourS a) -> BehaviourS a
switchS (h :-> t) e = h :-> fmap (either (`switchS` e) id) (first t e) 

whenJustS :: BehaviourS (Maybe a) -> BehaviourS (Event a)
whenJustS (h :-> t) = 
  let t' = fmap whenJustS t
  in case h of
   Just x  -> pure x :-> t'
   Nothing -> (t' >>= headB) :-> (t' >>= tailB)

seqS :: BehaviourS x -> BehaviourS a -> BehaviourS a
seqS  s@(sh :-> st) b@(h :-> t) = 
  (sh `seq` h) :-> fmap (either (`seqS` b) (s `seqS`)) (first st t) 

afterS :: BehaviourS a -> PastTime -> BehaviourS a
afterS (h :-> t) tm = case t `infoAt` tm of
   Just (te,b) -> b `afterS` max tm te
   Nothing     -> h :-> t


sampleNow :: Behaviour a -> IO a
sampleNow b = (\t -> headB $ b `after` t) <$> getTime

newtype Behaviour a = Behaviour { after :: PastTime -> BehaviourS a}

instance Monad Behaviour where
  return x = Behaviour $ const (return x)
  m >>= f  = behaviour $ \t -> bind (m `after` t) f `after` t where
   bind :: BehaviourS a -> (a -> Behaviour b) -> Behaviour b
   bind (h :-> t) f = f h `switch` fmap (`bind` f) t

instance MonadFix Behaviour where
  mfix f = let b = f (headB b) `after` bigBang in Behaviour (b `afterS`)
switch m e = Behaviour $ \t -> case e `infoAt` t of
     Just (te,a) -> a `after` max t te
     Nothing     -> (m `after` t) `switchS` cont t e 
 where cont :: PastTime -> Event (Behaviour a) -> Event (BehaviourS a)
       cont t e = fmap (\(te,a) -> a `after` max te t) (withTime e)

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust b = behaviour $ \t -> whenJustS (b `after` t)

seqB :: Behaviour x -> Behaviour a -> Behaviour a
seqB s r = behaviour $ \t -> (s `after` t) `seqS` (r `after` t)

updateSelf :: Behaviour a -> Behaviour a 
updateSelf b = Behaviour $ \t -> loop (b `after` t) where
  loop (h :-> t) = h :-> fmap (loop . update) (withTime t)
  update (t,a) = (b `after` t) `seq` a
                              
memoB :: (PastTime -> BehaviourS a) -> Behaviour a
memoB f = Behaviour $ unsafePerformIO $ liftM f' (newIORef Nothing) where
  f' r t = unsafePerformIO $
   do v <- readIORef r
      let v' = nextVal t v
      writeIORef r (Just (t,v'))
      return v'
  nextVal t v = case v of
   Nothing     -> f t
   Just (tp,v) -> update t tp v
  update t tp v = case compare tp t of
     GT -> error "Non increasing sampling!"
     EQ -> v
     LT -> v `afterS` t

behaviour = updateSelf . memoB


instance Functor Behaviour where
  fmap = liftM 

instance Applicative Behaviour where
  pure = return
  (<*>) = ap
