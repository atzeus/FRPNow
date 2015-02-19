{-# LANGUAGE TypeOperators,TypeSynonymInstances,FlexibleInstances, MultiParamTypeClasses,GeneralizedNewtypeDeriving #-}
module FRPNow(Event,Behavior,SpaceTime, Now, switch, whenJust, async, runFRP) where

import Impl.TimeEnv(SpaceTime) 
import qualified Impl.TimeEnv as I
import Swap
import Control.Applicative
-- this gives nicer type errors

newtype Behavior a = Bh { fromBh :: I.Behavior a } deriving (Functor,Applicative,Monad)
newtype Event    a = Ev { fromEv :: I.Event a    } deriving (Functor,Applicative,Monad)
type Now = (Behavior :. SpaceTime)

switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch (Bh a) (Ev b) = Bh (a `I.switch` (fromBh <$> b))

whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust (Bh b) = Bh (Ev <$> I.whenJust b)

instance Swap Behavior SpaceTime where
  swap x = Bh (swap (fromBh <$> x))

instance Swap Now Event where
  swap (Ev x) = inow2now $ Ev <$> (swap (now2inow <$> x))

now2inow :: Now a -> I.Now a
now2inow = Close . fromBh . open

inow2now :: I.Now a -> Now a
inow2now = Close . Bh . open

async :: IO a -> Now (Event a)
async m = inow2now $ Ev <$> I.async m

runFRP :: Now (Event a) -> IO a
runFRP m = I.runFRP (fromEv <$> now2inow m)
