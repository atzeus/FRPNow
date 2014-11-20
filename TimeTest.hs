

import Syntactic.Behaviour
import Syntactic.Time

import Control.Concurrent
import Control.Applicative
import Control.Monad
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
     e <- count1 10000
     v <- cur $ whenJust ( (\x -> if x > 20 then Just 1 else Nothing) <$> b)
     let b' = b `switch` (fmap return v)
     let f = (,) <$> b' <*> e
     m <- cur $ whenJust ( (\x -> if x == (1,2000) then Just (0,0) else Nothing) <$> f)
     showChanges (f `switch` (fmap pure m))
     return never

showChanges :: (Eq a, Show a) => Behaviour a -> Now ()
showChanges b = loop where
 loop = do v <- cur b
           syncIO $ putStrLn (show v)
           e <- cur $ whenJust (toJust v <$> b)
           e' <- planIO (fmap (const (loop)) e)
           return ()
  where  toJust v x = if v == x then Nothing else Just x

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


