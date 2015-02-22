


import FRPNow

import Control.Concurrent
import Control.Applicative
import Control.Monad hiding (when)
import System.Time

main = do runFRP test  

test = 
  do b <- count 50000 
     e <- count 25000
     let c = (+) <$> b <*> e
     cur $ when ((10000 <=) <$> c)

count :: Int -> Now (Behavior Int)
count delay = loop 0 where
  loop i = 
    do e <- async (threadDelay  delay)
       e' <- plan (loop (i+1) <$ e)
       return (pure i `switch` e')


