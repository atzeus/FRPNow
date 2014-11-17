{-# LANGUAGE  MultiParamTypeClasses , GeneralizedNewtypeDeriving, TypeOperators #-}
module Base.InternalIO where

import Base.Time
import Base.IVar
import Base.Event

import Control.Concurrent

asyncIO :: IO a -> Present (Event a)
asyncIO m = 
  do t <- doSync newPrimitiveTime 
     v <- doSync newIVar 
     doSync $ forkIO $ 
           do a <- m
              writeIVar v a
              timeIsNow t
     return (silentBlockVal v :@ timeOf t)


planIO :: Event (Present a) -> Present (Event a)
planIO (s :@ t) =   
  do v <- doSync newIVar 
     asap t $ do x  <- s
                 doSync $ writeIVar v x
     return (silentBlockVal v :@ t)

{-
curIO :: Behaviour a -> Present a
curIO b = sampleNow b 
-}

