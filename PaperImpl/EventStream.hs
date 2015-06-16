module EventStream where
import FRPNow
import Lib
import Control.Applicative
import Control.Monad

newtype Stream a = S { next :: B (E a) }

repeatIO :: IO a -> Now (Stream a)
repeatIO m = S <$> loop where
  loop = do  h  <- async m
             t  <- planNow (loop <$ h)
             return (pure h `switch` t)

snapshots :: B a -> Stream () -> Stream a
snapshots b (S s) = S $ do  e       <- s
                            snapshot b e

catMaybesStream :: Stream (Maybe a) -> Stream a
catMaybesStream (S s) = S loop where
  loop = do  e <- s
             join <$> plan (nxt <$> e)
--  nxt :: Maybe a -> B (E a)
  nxt (Just a) = return (return a)
  nxt Nothing  = loop

