{-# LANGUAGE TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.Event(Time(..),Event, makeEvent,never, first,runEv) where

import Control.Applicative
import Control.Monad
import Data.TIVar
import Control.Concurrent.MVar
import System.IO.Unsafe
import Debug.Trace
data Time s = MinBound | Time (Round s) deriving (Ord,Eq)

data EvState s a = After (Time s) (Event s a) 
                 | Occurred (Time s) a

newtype Event s a = E { runEv' :: Time s -> EvState s a }

again :: EvState s a -> Event s a
again es = 
  let x = E $ \t ->
       case es of
         Occurred t' _ 
           | t' <= t   -> es
           | otherwise -> After t x
         After t' e 
           | t > t'    -> runEv' e t
           | otherwise -> After t x
  in x

runEv :: Event s a -> Time s -> Maybe (Time s,a)
runEv e t = case runEv' e t of
              Occurred t' a -> Just (t',a)
              After  _ _   -> Nothing

makeEvent :: (Round s -> Maybe (Round s, a)) -> Event s a
makeEvent f = 
  let x = E $ \t -> 
            case t of
              MinBound -> After MinBound x
              Time ts -> case f ts of
                  Just (t',a) -> Occurred (Time t') a
                  Nothing    -> After (Time ts) x
  in x

never :: Event s a
never = E $ const $ After MinBound never

instance Monad (Event s) where
  return x = E $ const (Occurred MinBound x)
  m >>= f  = memo $ bind m f where
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

first :: Event s a -> Event s a -> Event s a
first l r =  memo $ first' l r where 
  first' l r = E $ \t -> 
    case runEv' r t of
     Occurred tr vr -> 
         case prev l t of
           Occurred tl vl -> if tl < tr then Occurred tl vl else Occurred tr vr
           _              ->  Occurred tr vr
     After dl r' -> 
       case runEv' l t of
           Occurred tl vl -> Occurred (max dl tl) vl
           After dr l'     -> After (max dl dr) (first' l' r')
  prev e t = case t of
         MinBound -> After MinBound e
         Time ts -> runEv' e (Time $ prevRound ts)


