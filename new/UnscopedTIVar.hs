{-# LANGUAGE LambdaCase  #-}
module UnscopedTIVar(Round, Clock, TIVar, newTIVar, writeTIVar, curRound, endRound,observeAt, newClock) where

-- TIVars without the scope type parameter
-- less safe, but easier on the eyes

import Control.Concurrent.MVar
import Control.Applicative
import System.IO.Unsafe
import Data.Unique

data Round = Round Unique Integer 

instance Eq Round where
  (Round lu lt) == (Round ru rt) | lu == ru  = lt == rt
                                 | otherwise = error "Rounds not from same clock!"

instance Ord Round where
  compare (Round lu lt) (Round ru rt)
     | lu == ru  = compare lt rt
     | otherwise = error "Rounds not from same clock!"

data TIVar a = TIVar Unique (MVar (Either Clock (Round, a)))
data Clock = Clock Unique (MVar Integer)


newTIVar :: Clock -> IO (TIVar a)
newTIVar c@(Clock u _) = TIVar u <$> newMVar (Left c)

writeTIVar :: TIVar a -> a -> IO ()
writeTIVar (TIVar _ m) a =
  takeMVar m >>= \case
    Left (Clock u c) -> do i <- takeMVar c
                           putMVar m (Right (Round u (i + 1), a))
                           putMVar c i
    Right _ -> error "written to tivar twice!"

curRound :: Clock -> IO Round
curRound (Clock u c) = Round u <$> readMVar c

endRound :: Clock -> IO ()
endRound (Clock u c) = do i <- takeMVar c
                          putMVar c (i+1)


observeAt :: TIVar a -> Round -> Maybe a
observeAt (TIVar uv m) (Round ur t) 
  | uv /= ur = error "Observation of TIVar from another context!"
  | otherwise = unsafePerformIO $ 
  do v <- readMVar m 
     return $ case v of
      Right (Round _ t',a) | t' <= t -> Just a
      _                      -> Nothing 
{-# NOINLINE observeAt #-}     


newClock :: IO Clock
newClock = Clock <$> newUnique <*> newMVar 0

