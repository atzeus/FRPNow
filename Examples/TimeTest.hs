


import FRPNow

import Control.Concurrent
import Control.Applicative
import Control.Monad hiding (when)
import System.Time

n = 11000
main = runNow test  

test :: Now (Event ())
test = do  b <- count
           sample (when ((n ==) <$> b))

count :: Now (Behavior Int)
count = loop 0 where
  loop i = 
    do e <- async (return ())
       e'<- plan (loop (i+1) <$ e)
       return (pure i `switch` e')

