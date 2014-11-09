{-# LANGUAGE TupleSections #-}
module Event where

import Past
import Future
import Control.Monad
import Control.Applicative
import Data.Maybe
import System.IO.Unsafe
import Data.IORef

newtype Event a = Event { infoAt :: PastTime -> Maybe (PastTime,a) }

liftPast :: PastTime -> a -> Event a
liftPast p a = Event $ \t -> if t >= p then Just (p,a) else Nothing 

liftFuture :: Future a -> Event a
liftFuture f = Event $ futureInfoAt f

never :: Event a
never = Event $ const Nothing


withTime :: Event a -> Event (PastTime,a)
withTime e = Event $ \t -> fmap (\(t,a) -> (t,(t,a))) $ e `infoAt` t

instance Monad Event where
  return x = Event $ const (Just (bigBang,x))
  m  >>= f = memoEv $ \t ->
      do (ta,a) <- m   `infoAt` t
         (tb,b) <- f a `infoAt` t
         return (max ta tb, b)

first :: Event a -> Event b -> Event (Either a b)
first l r = memoEv $ \t ->
 case (l `infoAt` t, r `infoAt` t) of
       (Just (ta,a), Just (tb,b) ) 
            | tb <= ta  -> Just (tb,Right b)
            | otherwise -> Just (ta,Left  a)
       (Just (ta,a), _) -> Just (ta,Left  a)
       (_, Just (tb,b)) -> Just (tb,Right b)
       (_, _ )          -> Nothing

memoEv :: (PastTime ->  Maybe (PastTime,a)) -> Event a 
memoEv f = Event $ unsafePerformIO $ liftM f' (newIORef (bigBang,Nothing)) where
  f' r t = unsafePerformIO $
      do (tp,v) <- readIORef r
         if tp >= t || isJust v
         then return (fmap (tp,) v)
         else let res = f t
              in writeIORef r (t,fmap snd res) >> return res 

instance Functor Event where
  fmap = liftM

instance Applicative Event where
  pure = return
  (<*>) = ap
  
