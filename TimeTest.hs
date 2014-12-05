{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import Control.FRPNow

import Control.Concurrent
import Control.Applicative
import Control.Monad hiding (when)
import Control.Concurrent.MVar 
import System.IO
main = do runNow (testb )

{-
test2 =  
   do e1 <- asyncIO (threadDelay  1000000 >> return 2)
      e2 <- asyncIO (threadDelay  2000000 >> return 3)
      e3 <- asyncIO (threadDelay  3000000 >> return 4)
      -- planIO (syncIO . print . show <$> ( (+) <$> ((+) <$> e3 <*> e2) <*> e1))
      planIO (syncIO . print . show <$> first e1 e2)
      return ()
-}
testb = 
  do b <- count1 500000
     e <- cur $ when ((== 5) <$> b)
     --e <- count1 500000
     --v <- cur $ whenJust ( (\x -> if x > 3000 then Just 1 else Nothing) <$> b)
     --let b' = b `switch` (fmap return v)
     --let f = (,) <$> b' <*> e
     --m <- cur $ whenJust ( (\x -> if x == (1,10000) then Just (0,0) else Nothing) <$> f)
--     showChanges (f `switch` (fmap pure m))
--     showChanges b
--     bc <- cur $ bla e
     showChanges (bla e)
     return never


bla :: Event () -> Behaviour Int
bla e1 = mdo e2 <- cur $ when $ (== 1) <$> b
             let  b = (pure 1) `switch` (pure 2 <$ e2)
             b
{-
bla e1 = let e = when $ (== 1) <$> b
             b = e >>= \e2 ->  pure 0  `switch` (pure 2 <$ e2)
         in b
-}
sampleEvery :: (Eq a, Show a) => Int -> Behaviour a -> Now ()
sampleEvery delay b = loop where
 loop = do v <- cur b
           syncIO $ putStrLn (show v)
           e <- asyncIO (threadDelay  delay)
           e' <- planIO (fmap (const (loop)) e)
           return ()



count1 :: Int -> Now (Behaviour Int)
count1 delay = loop 0 where
  loop i = 
    do e <- asyncIO (threadDelay  delay)
       e' <- planIO (fmap (const (loop (i+1))) e)
       return (pure i `switch` e')


