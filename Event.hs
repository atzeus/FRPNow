
{-# LANGUAGE  GADTs, ExistentialQuantification,TypeOperators,Rank2Types, ScopedTypeVariables #-}

module Event where

import Util.SingleWriteIORef
import Data.IORef
import Data.Maybe
import Control.Monad
import Control.Applicative
data Time = MinBound | Time Integer | MaxBound deriving (Eq, Ord)


nextTime :: Time -> Time
nextTime (Time t) = Time (t + 1)


data Event p a where
  Never   :: Event p a
  Occured :: Time -> a -> Event p a
  After   :: Time -> Event p a -> Event p a
  BindE   :: Event p a -> (a -> Event p b) -> Event p b
  Wait    :: p a -> Event p a
  Race    :: Event p a -> Event p b -> Event p (Either a b)

waitOn :: p a -> Event p a
waitOn p = Wait p

never = Occured MaxBound undefined
race = Race
wait = Wait

instance Monad (Event p) where
  return = Occured MinBound
  (>>=)  = BindE

makeAtTime :: Time -> a -> Event p a
makeAtTime t a = Occured t a

getEvent :: Event p a -> Maybe a
getEvent (Occured t a) = Just a
getEvent _             = Nothing

updateEvent :: forall p a. (forall a. p a -> Maybe a) -> Time -> Event p a -> Event p a
updateEvent round now = loop MinBound where
  loop :: Time -> Event p b -> Event p b
  loop tm e = case e of
    BindE (BindE e f) g   -> loop tm (BindE e (\x -> f x >>= g))
    After tl (After tr e) -> loop tm (After (max tl tr) e)
    Occured t a  -> Occured (max t tm) a
    After t e    -> loop (max tm t) e
    BindE e f    -> case loop tm e of
                      Occured t a | t <= now -> loop (max tm t) (f a)
                      e'          -> BindE e' f
    Wait p       -> case round p of
                        Just a  -> Occured (max tm now) a
                        Nothing -> After tm (Wait p)
    Race l r     -> case (loop tm l,loop tm r) of
                         (Occured tl l, Occured tr r) 
                             |  tr <= tl  -> Occured (max tm tr) (Right r)
                             |  otherwise -> Occured (max tm tl) (Left  l)
                         (_, Occured t r) | t <= now -> Occured (max tm t) (Right r)
                         (Occured t l, _) | t <= now -> Occured (max tm t) (Left  l)
                         (l',r')          -> After tm (Race l' r')



instance Functor (Event p) where
  fmap f a = a >>= return . f

instance Applicative (Event p) where
  pure = return
  f <*> g = do x <- f ; y <- g ; return (x y)


