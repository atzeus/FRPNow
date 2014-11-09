module Past(bigBang,PastTime,withCurTime) where

import Control.Concurrent
import Control.Concurrent.MVar
import System.Clock 
import System.IO.Unsafe

data    PastTime = BigBang 
                 | PastTime TimeSpec deriving (Ord,Eq) -- abstract

bigBang :: PastTime
bigBang = BigBang

withCurTime :: (PastTime -> IO ()) -> IO ()
withCurTime f = 
  do tl <- takeMVar globalClockLowerbound
     t <- higherTime tl
     f (PastTime t)
     putMVar globalClockLowerbound t
  where higherTime tl = loop where
         loop = do t <- getTime Monotonic
                   if tl == t
                   then yield >> loop
                   else return t
             

globalClockLowerbound :: MVar TimeSpec
globalClockLowerbound = unsafePerformIO $ newMVar (TimeSpec minBound minBound)


