

import Base.InternalIO
import Base.Event

import Control.Concurrent
import Control.Applicative
import Control.Monad

main = test2

test2 = do e1 <- asyncIO (threadDelay  1000000 >> return 2)
           e2 <- asyncIO (threadDelay  2000000 >> return 3)
           e3 <- asyncIO (threadDelay  3000000 >> return 4)
           --e <- planIO (print . show <$> withTime (first e1 e2))
           v <- waitIO (never >> return ())
           print (show v)
           --waitIO e
          


