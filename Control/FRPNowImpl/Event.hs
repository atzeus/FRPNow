{-# LANGUAGE TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.Event(Time(..),Event, makeEvent,never, first,runEv) where

import Control.Applicative
import Control.Monad
import Control.FRPNowImpl.ASync
import Control.Concurrent.MVar
import System.IO.Unsafe

data Time s = MinBound | Time (Timestamp s) deriving (Ord,Eq)

data EvState s a = Delay (Time s) (Event s a) 
                 | Occured (Time s) a

newtype Event s a = E { runEv' :: Time s -> EvState s a }

runEv :: Event s a -> Time s -> Maybe (Time s,a)
runEv e t = case runEv' e t of
              Occured t a -> Just (t,a)
              Delay _ _   -> Nothing

makeEvent :: (Timestamp s -> Maybe (Timestamp s, a)) -> Event s a
makeEvent f = E $ \t -> 
  case t of
    MinBound -> Delay MinBound (makeEvent f)
    Time t -> case f t of
                Just (t,a) -> Occured (Time t) a
                Nothing    -> Delay MinBound (makeEvent f)


never = E $ const $ Delay MinBound never


instance Monad (Event s) where
  return x = E $ const (Occured MinBound x)
  m >>= f  = memo $ bind m f


bind m f = E $ \t -> 
    case runEv' m t of
     Delay d m'   -> Delay d (bind m' f)
     Occured t' x -> 
        case runEv' (f x) t of
           Occured t'' y -> Occured (max t' t'') y
           Delay t'' e   -> Delay (max t' t'') e

instance Functor (Event s) where
  fmap = liftM
instance Applicative (Event s) where
  pure = return
  (<*>) = ap

first l r = memo $ first' l r

first' :: Event s a -> Event s a -> Event s a
first' l r = E $ \t -> 
  case runEv' r t of
   Occured tr vr -> case prev l t of
                      Occured tl vl -> if tl < tr then Occured tl vl else Occured tr vr
                      _             ->  Occured tr vr
   Delay dl r' -> case runEv' l t of
                Occured tl vl -> Occured (max dl tl) vl
                Delay dr l'     -> Delay (max dl dr) (first' l' r')
 where prev e t = case t of
         MinBound -> Delay MinBound e
         Time t -> runEv' e (Time $ prevTimestamp t)

memo :: Event s a -> Event s a
memo e = E $ \t -> unsafePerformIO $ runMemo t where
  mvar = unsafePerformIO $ newMVar (Delay MinBound e)
  {-# NOINLINE mvar #-}  
  runMemo t = 
    do v <- update t <$> takeMVar mvar 
       putMVar mvar v
       return v
  update t (Delay d e) = case runEv' e t of
                           Occured t' a -> Occured (max d t') a
                           Delay t' e   -> Delay (max d t') e
  update t (Occured t' a) = Occured t' a
{-# NOINLINE memo #-}  

