
{-# LANGUAGE  GADTs, ExistentialQuantification,TypeOperators,Rank2Types, ScopedTypeVariables #-}

module Event where

import Util.SingleWriteIORef
import Data.IORef
import Data.Maybe
import TimeIVar
import Control.Monad
import Control.Applicative


data Time s = MinBound | Time (TimeStamp s) | MaxBound deriving (Eq, Ord)

instance Bounded (Time s) where
  minBound = MinBound
  maxBound = MaxBound  

data Event s p a where
  Occured :: Time s -> a -> Event s p a
  After   :: Time s -> Event s p a -> Event s p a
  BindE   :: Event s p a -> (a -> Event s p b) -> Event s p b
  Wait    :: p a -> Event s p a
  Race    :: Event s p a -> Event s p b -> Event s p (Either a b)

waitOn :: p a -> Event s p a
waitOn p = Wait p

never = Occured maxBound undefined
race = Race
wait = Wait

instance Monad (Event s p) where
  return = Occured minBound
  (>>=)  = BindE

makeAtTime :: Time s -> a -> Event s p a
makeAtTime t a = Occured t a

getEvent :: Event s p a -> Maybe a
getEvent (Occured t a) = Just a
getEvent _             = Nothing

updateEvent :: forall p a s. (forall a. p a -> Maybe a) -> Time s -> Event s p a -> Event s p a
updateEvent round now = loop minBound where
  loop :: Time s -> Event s p b -> Event s p b
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



instance Functor (Event s p) where
  fmap f a = a >>= return . f

instance Applicative (Event s p) where
  pure = return
  f <*> g = do x <- f ; y <- g ; return (x y)


