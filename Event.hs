
{-# LANGUAGE  TupleSections,GADTs, ExistentialQuantification,TypeOperators,Rank2Types, ScopedTypeVariables #-}

module Event(Time, timeMinBound, fromStamp, Event, never, getAt, delay, fromTIVar, race, memoEv) where

import TimeIVar
import Data.IORef
import System.IO.Unsafe
import Control.Monad
import Control.Applicative

data Time s = MinBound | Time (TimeStamp s) | MaxBound deriving (Eq, Ord)

timeMinBound = MinBound
fromStamp    = Time

newtype Event s a = Ev { getAt :: Time s -> Maybe (Time s, a) }

never = Ev $ const Nothing

fromTIVar :: TIVar s a -> Event s a 
fromTIVar v = Ev convTime where
  convTime MinBound   = Nothing
  convTime t@(Time s) = fmap (t,) (v `tvarAt` s)
  convTime MaxBound   = undefined

delay :: Time s -> Event s a -> Event s a
delay tm (Ev f) = Ev $ \t -> if t >= tm 
                             then do (t',a) <- f t
                                     return (max t' tm,a)
                              else Nothing

instance Monad (Event s) where
  return x = Ev $ const $ Just (MinBound,x)
  Ev m >>= f = memoEv $ \t -> 
       do (ta,a) <- m t 
          (tb,b) <- getAt (f a) t
          return (max ta tb, b)

race :: Event s a -> Event s b -> Event s (Either a b)
race l r = memoEv $ \t -> 
      case (l `getAt` t, r `getAt` t) of
        (Just (ta,a) , Just (tb,b)) 
            | ta < tb    -> Just (ta, Left  a)
            | otherwise  -> Just (tb, Right b)
        (Just (ta,a), _) -> Just (ta, Left  a)
        (_, Just (tb,b)) -> Just (tb, Right b)
        _                -> Nothing

-- only safe on events

memoEv :: (Time s -> Maybe (Time s, a)) -> Event s a
memoEv f = Ev $ unsafePerformIO $ liftM f' (newIORef Nothing) where
  f' r t = unsafePerformIO $
      do v <- readIORef r
         case v of
            Just (t',a) | t' <= t -> return $ Just (t',a)
            _ -> let res = f t
                 in writeIORef r res >> return res 


instance Functor (Event s) where
  fmap = liftM

instance Applicative (Event s) where
  pure = return
  (<*>) = ap

