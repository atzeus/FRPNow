{-# LANGUAGE LambdaCase, Rank2Types  #-}
module Data.TIVar where


import Control.Concurrent.MVar
import Control.Applicative
import System.IO.Unsafe

data Round s = Round Integer deriving (Ord,Eq,Show)

newtype TIVar s a = TIVar (MVar (Either (Clock s) (Round s, a)))
newtype Clock s = Clock (MVar Integer)


prevRound :: Round s -> Round s 
prevRound (Round i) = Round (i - 1)

newTIVar :: Clock s -> IO (TIVar s a)
newTIVar c = TIVar <$> newMVar (Left c)

writeTIVar :: TIVar s a -> a -> IO ()
writeTIVar (TIVar m) a =
  takeMVar m >>= \case
    Left (Clock c) -> do i <- takeMVar c
                         putMVar m (Right (Round (i + 1), a))
                         putMVar c i
    Right _ -> error "written to tivar twice!"

curRound :: Clock s -> IO (Round s)
curRound (Clock c) = Round <$> readMVar c

endRound :: Clock s -> IO ()
endRound (Clock c) = do i <- takeMVar c
                        putMVar c (i+1)

observeAt :: TIVar s a -> Round s -> Maybe (Round s,a)
observeAt (TIVar m) t = unsafePerformIO $ 
  do v <- readMVar m 
     return $ case v of
      Right (t',a) | t' <= t -> Just (t',a)
      _                      -> Nothing 
{-# NOINLINE observeAt #-}     


withClock :: (forall s. Clock s -> IO a) -> IO a
withClock m = (Clock <$> newMVar 0) >>= m

unsafeWithClock :: (Clock s -> IO a)  -> IO a
unsafeWithClock m = (Clock <$> newMVar 0) >>= m

