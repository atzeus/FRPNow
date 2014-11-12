{-# LANGUAGE  MultiParamTypeClasses , GeneralizedNewtypeDeriving, TypeOperators #-}
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
import FunctorCompose

step :: a -> Event (Behaviour a) -> Behaviour a
step a s = pure a `switch` s

toBehaviour :: Event (Behaviour a) -> Behaviour (Maybe a)
toBehaviour e = Nothing `step` fmap (fmap Just) e

plan :: Event (Behaviour a) -> Behaviour (Event a)
plan = whenJust . toBehaviour

type Now = Behaviour :. SpaceTimeM

instance FlipF Event SpaceTimeM where flipF = continueST
instance FlipF Event Behaviour where flipF =  plan

newtype SpaceTimeM a = SpaceTimeM { runSpaceTime :: IO a } deriving (Monad,Applicative,Functor)

doAt :: IO a -> Now (Event a)
doAt m = Comp $ pure (SpaceTimeM $ startJob m)

continueDo :: Event (Now a)-> Now (Event a)
continueDo = flipF

continueST :: Event (SpaceTimeM a) -> SpaceTimeM (Event a)
continueST = SpaceTimeM . startFork

startJob :: IO a -> IO (Event a)
startJob m = 
  do t <- newPrimitiveTime 
     v <- newIVar 
     forkIO $ do a <- m
                 writeIVar v a
                 timeIsNow t
     return (valIVar v :@ timeOf t)

startFork :: Event (SpaceTimeM a) -> IO (Event a)
startFork (s :@ t) =   
  do v <- newIVar 
     forkIO $ do tp <- waitFor t
                 x  <- runSpaceTime s
                 writeIVar v x
     return (valIVar v :@ t)

runFRP :: Behaviour (SpaceTimeM a) -> IO a
runFRP b = sampleNow b >>= runSpaceTime


