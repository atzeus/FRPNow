import FRPNow 
import Lib
import Control.Applicative

n = 11000

main = runNow (test n)

test :: Int -> Now (E ())
test n = do  b <- count
             sample (when ((n ==) <$> b))

count :: Now (B Int)
count = loop 0 where
  loop i =  do  e <- async (return ())
                e'<- planNow (loop (i+1) <$ e)
                return (pure i `switch` e')
