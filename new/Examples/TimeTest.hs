


import Impl.FRPNow
import Lib.Lib

import Control.Concurrent
import Control.Applicative
import Control.Monad hiding (when)

main = do runFRP testb   >>= putStrLn . show


testb = 
  do b <- count1 500000 :: Now (Behavior Int)
     e <- count1 250000 :: Now (Behavior Int)
     let c = (+) <$> b <*> e
     showChanges ((,,) <$> c <*> b <*> e)
     cur $ when ((100 <=) <$> c)

count1 :: Int -> Now (Behavior Int)
count1 delay = loop 0 where
  loop i = 
    do e <- async (threadDelay  delay)
       e' <- plan (loop (i+1) <$ e)
       return (pure i `switch` e')

{-
bla e1 = let e = when $ (== 1) <$> b
             b = e >>= \e2 ->  pure 0  `switch` (pure 2 <$ e2)
         in b

bla :: Event Int -> Behavior (Event Int)
bla e = loop where
  loop = occ e >>= \x -> case x of
           Just a -> pure (pure 1)
           Nothing -> do e' <- join <$> plan (loop <$ e) 
                         pure e' `switch` (loop <$ e')
-}
