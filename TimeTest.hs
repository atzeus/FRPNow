{-# LANGUAGE LambdaCase,NoMonomorphismRestriction,TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import Control.FRPNow


import Control.Concurrent
import Control.Applicative
import Control.Monad hiding (when)
import Control.Concurrent.MVar 
import System.IO
main = do runNow (testb )   >>= putStrLn . show

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
  do --e <- asyncIO (threadDelay 1000000 >> return 1)
     --e2 <- asyncIO (threadDelay 10000 >> return 2)
     
     {-e2 <- asyncIO (threadDelay 2000000 >> return 2)
     a <- planIO (return 3 <$ e)
     -}
--     a <- firstObsNow e e2
     b <- count1 500000 :: Now (Behaviour Int)
     -- b2 <- count1 700000 :: Now (Behaviour Int)
     v <- cur $ whenJust ( (\x -> if x > 10 then Just 1 else Nothing) <$> b)
     e <- planIOWeak ((syncIO (putStrLn "Bla") >> return 10) <$ v)
{-
     let b' = b `switch` (fmap return v)
     let f =  (+) <$> b <*> b'
     v2 <- cur $ whenJust ( (\x -> if x > 30 then Just 1 else Nothing) <$> f)

     e' <- cur $ bla e
  -}   
--     showChanges $ (,) <$> b <*> getNow v
     return  v -- (never :: Event Int)
     {-
     let evs = b `sampleOn` (repeatEv $ change b)
     let isEven x = x `mod` 2 == 0
     let evs' = filterJusts $ (\x -> if isEven x then Just x else Nothing) <$> evs
     -}
     --let getEm x = if x > 5 then Just x else Nothing
     --

--     v <- cur $ whenJust ( (\x -> if x > 10 then Just 1 else Nothing) <$> b)
--     let b' = b `switch` (fmap return v)
     --let f = (,) <$> b' <*> e
     --m <- cur $ whenJust ( (\x -> if x == (1,10000) then Just (0,0) else Nothing) <$> f)
--     showChanges (f `switch` (fmap pure m))
--     sampleEvery 500000  
  --   bc <- cur $ bla e
     --let f =  (+) <$> b <*> b'

     
     --sampleEvery 50000 b
  --   e <- cur $ when ((== (21)) <$> f)

{-

bla :: Event () -> Behaviour Int
bla e1 = mdo e2 <- cur $ when $ (== 1) <$> b
             let  b = (pure 1) `switch` (pure 2 <$ e2)
             b
-}
{-
bla e1 = let e = when $ (== 1) <$> b
             b = e >>= \e2 ->  pure 0  `switch` (pure 2 <$ e2)
         in b
-}

bla :: Event Int -> Behaviour (Event Int)
bla e = loop where
  loop = getNow e >>= \case
           Just a -> pure (pure 1)
           Nothing -> do e' <- join <$> plan (loop <$ e) 
                         pure e' `switch` (loop <$ e')
           
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


