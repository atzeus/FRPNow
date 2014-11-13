{-# LANGUAGE  MultiParamTypeClasses , GeneralizedNewtypeDeriving, TypeOperators #-}
module Base.InternalIO where

import Base.Behaviour
import Base.Time
import Base.IVar
import Base.Event

import Control.Concurrent

asyncIO :: IO a -> IO (Event a)
asyncIO m = 
  do t <- newPrimitiveTime 
     v <- newIVar 
     forkIO $ do a <- m
                 writeIVar v a
                 timeIsNow t
     return (valIVar v :@ timeOf t)

planIO :: Event (IO a) -> IO (Event a)
planIO (s :@ t) =   
  do v <- newIVar 
     forkIO $ do tp <- waitFor t
                 x  <- s
                 writeIVar v x
     return (valIVar v :@ t)

curIO :: Behaviour a -> IO a
curIO b = sampleNow b 


