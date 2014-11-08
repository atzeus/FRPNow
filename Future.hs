module Future where

import Past
import System.IO.Unsafe
import IVar
import Control.Monad

newtype Future a = Future (IVar (PastTime,a))

newFuture :: IO (Future a)
newFuture = liftM Future newIVar

futureIsNow :: Future a -> a -> IO ()
futureIsNow (Future r) a = 
  do t <- getTime 
     writeIVar r (t,a)

futureInfoAt :: Future a -> PastTime -> Maybe (PastTime,a)
futureInfoAt (Future r) t = unsafePerformIO $
  do v <- readIVar r
     case v of
       Just (tp,a) | tp <= t -> return (Just (tp,a))
       _                     -> return Nothing



