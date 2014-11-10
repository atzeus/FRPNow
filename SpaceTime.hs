{-# LANGUAGE  GeneralizedNewtypeDeriving, TypeOperators #-}
module SpaceTime where

import Behaviour
import Time
import IVar

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Event
import Data.Either

newtype SpaceTime a = SpaceTime { runSpaceTime :: IO a } deriving (Monad,Applicative,Functor)

startNow  :: IO a -> SpaceTime (Event a)
startNow = SpaceTime . startJob

continue :: Event (SpaceTime a) -> SpaceTime (Event a)
continue = SpaceTime . startFork

startJob :: IO a -> IO (Event a)
startJob m = 
  do t <- newPrimitiveTime 
     v <- newIVar 
     forkIO $ do a <- m
                 writeIVar v a
                 timeIsNow t
     return (valIVar v :@ timeOf t)

startFork :: Event (SpaceTime a) -> IO (Event a)
startFork (s :@ t) =   
  do v <- newIVar 
     forkIO $ do tp <- waitFor t
                 x  <- runSpaceTime s
                 writeIVar v x
     return (valIVar v :@ t)

runFRP :: Behaviour (SpaceTime a) -> IO a
runFRP b = sampleNow b >>= runSpaceTime


