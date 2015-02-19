{-# LANGUAGE TypeOperators,TypeSynonymInstances,FlexibleInstances, MultiParamTypeClasses,GeneralizedNewtypeDeriving #-}
module FRPNow(Event,Behavior, Now, never, switch, whenJust, curNow, async, runFRP, unsafeSyncIO) where

import Impl.TimeEnv(Now)
import qualified Impl.TimeEnv as I
import Swap
import Control.Applicative
-- this gives nicer type errors

newtype Behavior a = Bh { fromBh :: I.Behavior a } deriving (Functor,Applicative,Monad)
newtype Event    a = Ev { fromEv :: I.Event a    } deriving (Functor,Applicative,Monad)


never :: Event a
never = Ev I.never

switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch (Bh a) (Ev b) = Bh (a `I.switch` (fromBh <$> b))

whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust (Bh b) = Bh (Ev <$> I.whenJust b)

instance Swap Now Event where
  swap (Ev x) = Ev <$> (swap x)

async :: IO a -> Now (Event a)
async m = Ev <$> I.async m

curNow (Bh b) = I.curNow b

runFRP :: Now (Event a) -> IO a
runFRP m = I.runFRP (fromEv <$> m)

unsafeSyncIO :: IO a -> Now a
unsafeSyncIO = I.unsafeSyncIO
