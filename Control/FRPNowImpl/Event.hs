{-# LANGUAGE TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.Event(Time(..),Event(..), makeEvent, first) where

import Control.Applicative
import Control.Monad
import Control.FRPNowImpl.ASync

data Time s = MinBound | Time (Timestamp s) deriving (Ord,Eq)

newtype Event s a = E { runEv :: Time s -> Maybe (Time s, a) }

makeEvent :: (Timestamp s -> Maybe (Timestamp s, a)) -> Event s a
makeEvent f = E $ \t -> 
  case t of
    MinBound -> Nothing
    Time t -> (\(t,a) -> (Time t, a)) <$> f t

never = E $ const $ Nothing

instance Functor (Event s) where
  fmap f e = E $ \t -> case runEv e t of
                        Just (t,x) -> Just (t,f x)
                        Nothing  ->  Nothing

instance Applicative (Event s) where
  pure = return
  (<*>) = ap

instance Monad (Event s) where
  return x = E $ const (Just (MinBound,x))
  m >>= f  = E $ \t -> 
      -- maybe monad
      do (ta,a) <- runEv m t 
         (tx,x) <- runEv (f a) t
         Just (max ta tx, x)

first :: Event s a -> Event s a -> Event s a
first l r = E $ \t -> 
  case runEv r t of
    Just (tr,r) -> 
       Just $ case getPrev l t of
         Just (tl,l) -> if tr <= tl then (tr,r) else (tl,l)
         Nothing     -> (tr,r)
    Nothing -> runEv l t

 where getPrev x t =  
        case t of
         MinBound -> Nothing
         Time t -> runEv x (Time $ prevTimestamp t)
