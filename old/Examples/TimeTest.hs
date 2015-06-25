


import FRPNow

import Control.Concurrent
import Control.Applicative
import Control.Monad hiding (when)
import System.Time
import Lib.Time

n = 1100000
main = runNow testcb

test :: Now (Event ())
test = do  b <- count
           sample (when ((n ==) <$> b))

count :: Now (Behavior Int)
count = loop 0 where
  loop i =
    do e <- async (return ())
       e'<- plan (loop (i+1) <$ e)
       return (pure i `switch` e')

callEvery :: IO () -> Int -> IO ()
callEvery f n = f >> threadDelay n >> callEvery f n


testcb :: Now (Event ())
testcb = do s <- getClock (1.0 /30.0)
            showChanges s
            return never
