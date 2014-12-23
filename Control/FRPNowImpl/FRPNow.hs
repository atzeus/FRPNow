
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
module Control.FRPNowImpl.FRPNow(
  Event, never,evNow,
  Now,syncIO,asyncIO,planIO, planIOWeak, runNow,
  Behaviour,curIO, switch, whenJust) where

import Control.Monad
import Control.Applicative
import qualified Control.FRPNowImpl.Now as N
import qualified Control.FRPNowImpl.Event as E
import qualified Control.FRPNowImpl.Behaviour as B


newtype Now       a = N { getN :: N.Now       N.Global a } deriving (Functor,Monad,Applicative)
newtype Event     a = E { getE :: E.Event     N.Global a } deriving (Functor,Monad,Applicative)
newtype Behaviour a = B { getB :: B.Behaviour N.Global a } deriving (Functor,Monad,Applicative)

never :: Event a
never = E $ E.never

evNow :: Event a -> Now (Maybe a)
evNow (E e) = N $ N.evNow e

syncIO :: IO a -> Now a
syncIO n = N $ N.syncIO n

asyncIO :: IO a -> Now (Event a)
asyncIO m = N $ E <$> N.asyncIO m


planIO :: Event (Now a) -> Now (Event a)
planIO (E e) = N $ E <$> N.planIO (getN <$> e)

planIOWeak :: Event (Now a) -> Now (Event a)
planIOWeak (E e) = N $ E <$> N.planIOWeak (getN <$> e)

curIO :: Behaviour a -> Now a
curIO (B a) = N $ B.curIO a

switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch (B b) (E e) = B $ b `B.switch` (getB <$> e)

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust (B b) = B $ E <$> B.whenJust b


runNow :: Now (Event a) -> IO a
runNow (N n) = N.runFRP (getE <$> n)
