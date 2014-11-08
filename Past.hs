module Past(bigBang,PastTime,getTime) where

import Control.Concurrent
import Control.Concurrent.MVar
import System.Clock hiding (getTime)

import qualified System.Clock as Clock
import System.IO.Unsafe

data    PastTime = BigBang 
                 | PastTime TimeSpec deriving (Ord,Eq) -- abstract

bigBang :: PastTime
bigBang = BigBang

getTime :: IO PastTime
getTime = do tl <- takeMVar globalClockLowerbound
             t <- higherTime tl
             putMVar globalClockLowerbound t
             return (PastTime t)
  where higherTime tl = loop where
         loop = do t <- Clock.getTime Monotonic
                   if tl == t
                   then yield >> loop
                   else return t
             

globalClockLowerbound :: MVar TimeSpec
globalClockLowerbound = unsafePerformIO $ newMVar (TimeSpec minBound minBound)


