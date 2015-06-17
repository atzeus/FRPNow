import FRPNow 
import NowMaster
import Lib
import Control.Applicative

n = 11000

main = runNowMaster (test n)

test :: Int -> Now (E ())
test n = do b <- count 
            e <- sample (when ((n ==) <$> b))
            return e

count :: Now (B Int)
count = loop 0 where
  loop i =  do  e <- async (return ())
                e'<- planNow (loop (i+1) <$ e)
                return (pure i `switch` e')
