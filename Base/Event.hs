{-# LANGUAGE TupleSections #-}
module Base.Event where

import Base.Time
import Control.Monad
import Control.Applicative
import Data.Maybe
import System.IO.Unsafe

data Event a = a :@ Time


never = undefined :@ maxBound

instance Monad Event where
  return x = x :@ minBound
  (a :@ ta)  >>= f = let b :@ tb = f a
                     in b  :@ maxTime ta tb

withTime :: Event a -> Event (PastTime,a)
withTime e = (,) <$> evTime e <*> e

evTime :: Event a -> Event PastTime
evTime (a :@ t) = silentBlockTime t :@ t

first :: Event a -> Event b -> Event (Either a b)
first (a :@ ta) (b :@ tb) = 
  do t <- evTime (() :@ minTime ta tb )
     return $ case (ta `pastTimeAt` t, tb `pastTimeAt` t) of
           (Just l, Just r ) 
                 | r <= l    -> Right b
                 | otherwise -> Left a
           (Just l, _      ) -> Left a
           (_     , Nothing) -> Right b   

getEvent :: Event a -> Present (Maybe a)
getEvent (a :@ t) = 
  (\v -> if v then Just a else Nothing) <$> isNow t


instance Functor Event where
  fmap = liftM

instance Applicative Event where
  pure = return
  (<*>) = ap
  
