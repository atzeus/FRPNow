{-# LANGUAGE LambdaCase,NoMonomorphismRestriction,TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import FRPNow
import Lib.Lib

import Control.Concurrent
import Control.Applicative
import Control.Monad hiding (when)
import Control.Concurrent.MVar 
import System.IO
main = do runFRP testb   >>= putStrLn . show


testb = 
  do b <- count1 500000 :: Now (Behavior Int)
     e <- count1 250000 :: Now (Behavior Int)
     -- let c = (+) <$> b <*> e
     showChanges ((,) <$> b <*> e)
     {-ev <- cur $ when ((1 <=) <$> b)
     x <- cur $ (bla (10 <$ ev))
     return  (x :: Event Int)
     -}
     return (never :: Event Int)

count1 :: Int -> Now (Behavior Int)
count1 delay = loop 0 where
  loop i = 
    do 
       e <- async (threadDelay  delay)
       e' <- plan (fmap (const (loop (i+1))) e)
       return (pure i `switch` e')

bla e1 = let e = when $ (== 1) <$> b
             b = e >>= \e2 ->  pure 0  `switch` (pure 2 <$ e2)
         in b

{-
bla :: Event Int -> Behavior (Event Int)
bla e = loop where
  loop = occ e >>= \case
           Just a -> pure (pure 1)
           Nothing -> do e' <- join <$> plan (loop <$ e) 
                         pure e' `switch` (loop <$ e')
-}
