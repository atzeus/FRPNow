{-# LANGUAGE LambdaCase  #-}
module Impl.WxPrimEv(Round, Clock, PrimEv, newClock , curRound, roundEnd ,observeAt,getCallback ) where

import Data.IORef
import Control.Applicative
import System.IO.Unsafe
import Data.Unique
import Debug.Trace

data Clock    = Clock Unique (IORef Integer) (IORef Bool)
data Round    = Round Unique Integer
data PrimEv a = PrimEv Unique (IORef (Maybe (Round, a)))

newClock :: IO Clock
newClock = Clock <$> newUnique <*> newIORef 0 <*> newIORef False

-- give an event that occurs when the also
-- returned IO action is executed for the first time
-- calls to the function afterwards will give an error
-- useful for interfacing with callback-based
-- systems
getCallback :: Clock -> IO (PrimEv a, a -> IO ())
getCallback (Clock u round flag) =
  do mv <- newIORef Nothing
     return (PrimEv u mv, setValue mv)
  where setValue mv x =
         do
            i <- readIORef round
            v <- readIORef mv
            let v' = case v of
                      Just _ -> error "Already called callback!"
                      _      -> Just (Round u (i + 1), x)
            writeIORef mv v'
            writeIORef flag True



curRound :: Clock -> IO Round
curRound (Clock u c _) = Round u <$> readIORef c

roundEnd :: Clock -> IO Bool
roundEnd (Clock u c f) = readIORef f >>= \x ->
       if x 
       then do writeIORef f False
               modifyIORef c (+1)
               return True
       else return False


observeAt :: PrimEv a -> Round -> Maybe a
observeAt (PrimEv uv m) (Round ur t)
  | uv /= ur = error "Observation of TIVar from another context!"
  | otherwise = unsafePerformIO $
  do v <- readIORef m
     return $ case v of
      Just (Round _ t',a) | t' <= t -> Just a
      _                             -> Nothing

instance Eq Round where
  (Round lu lt) == (Round ru rt) | lu == ru  = lt == rt
                                 | otherwise = error "Rounds not from same clock!"

instance Ord Round where
  compare (Round lu lt) (Round ru rt)
     | lu == ru  = compare lt rt
     | otherwise = error "Rounds not from same clock!"
