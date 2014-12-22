{-# LANGUAGE TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.Event(Time(..),Event, makeEvent,never, first,runEv) where

import Control.Applicative
import Control.Monad
import Control.ASync
import Control.Concurrent.MVar
import System.IO.Unsafe

data Time s = MinBound | Time (Timestamp s) deriving (Ord,Eq)

data EvState s a = After (Time s) (Event s a) 
                 | Occurred (Time s) a

newtype Event s a = E { runEv' :: Time s -> EvState s a }

again :: EvState s a -> Event s a
again es = E $ \t ->
 case es of
  Occurred t' a 
     | t' <= t   -> es
     | otherwise -> After t (again es)
  After t' e 
     | t > t'    -> runEv' e t
     | otherwise -> After t' (again es)

runEv :: Event s a -> Time s -> Maybe (Time s,a)
runEv e t = case runEv' e t of
              Occurred t a -> Just (t,a)
              After  _ _   -> Nothing

makeEvent :: (Timestamp s -> Maybe (Timestamp s, a)) -> Event s a
makeEvent f = E $ \t -> 
  case t of
    MinBound -> After MinBound (makeEvent f)
    Time t -> case f t of
                Just (t,a) -> Occurred (Time t) a
                Nothing    -> After (Time t) (makeEvent f)


never = E $ const $ After MinBound never

instance Monad (Event s) where
  return x = E $ const (Occurred MinBound x)
  m >>= f  = memo $ bind m f


bind m f = E $ \t -> 
    case runEv' m t of
     After d m'   -> After d (bind m' f)
     Occurred t' x -> 
        case runEv' (f x) t of
           Occurred t'' y -> Occurred (max t' t'') y
           After    t'' e -> After (max t' t'') e

instance Functor (Event s) where
  fmap = liftM
instance Applicative (Event s) where
  pure = return
  (<*>) = ap

first l r = memo $ first' l r

first' :: Event s a -> Event s a -> Event s a
first' l r = E $ \t -> 
  case runEv' r t of
   Occurred tr vr -> case prev l t of
                      Occurred tl vl -> if tl < tr then Occurred tl vl else Occurred tr vr
                      _             ->  Occurred tr vr
   After dl r' -> case runEv' l t of
                Occurred tl vl -> Occurred (max dl tl) vl
                After dr l'     -> After (max dl dr) (first' l' r')
 where prev e t = case t of
         MinBound -> After MinBound e
         Time t -> runEv' e (Time $ prevTimestamp t)

memo :: Event s a -> Event s a
memo e = E $ \t -> unsafePerformIO $ runMemo t where
  mvar = unsafePerformIO $ newMVar (After MinBound e)
  {-# NOINLINE mvar #-}  
  runMemo t = 
    do es <- takeMVar mvar 
       let es' = runEv' (again es) t
       putMVar mvar es'
       return es'
{-# NOINLINE memo #-}  

