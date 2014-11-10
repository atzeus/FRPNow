{-# LANGUAGE  GADTs, ExistentialQuantification,TypeOperators, ScopedTypeVariables, Rank2Types #-}
module SyncPar where

import Util.TermM
import Time
import IVar

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Event
import Data.Either

start  :: IO a -> SyncPar (Event a)
start = prim . Start

fork :: Event (SyncPar a) -> SyncPar (Event a)
fork = prim . Fork

type SyncPar = TermM SyncParP
data SyncParP a where
  Start :: IO a -> SyncParP (Event a)
  Fork  :: Event (SyncPar a) -> SyncParP (Event a)



runSyncPar :: SyncPar a -> IO a
runSyncPar m = interpret bind return where
  bind (Start m) = startJob m
  bind (Fork e)  = 

startJob :: IO a -> IO (Event a)
startJob m = 
  do t <- newPrimitiveTime 
     v <- newIVar 
     forkIO $ do a <- m
                 writeIVar v a
                 timeIsNow t
     return (valIVar v :@ timeOf t)


