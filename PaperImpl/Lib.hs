module Lib where

import FRPNow 
import Control.Applicative
-- Code from paper that is not implementation code


-- Section 3.1

change :: Eq a => B a -> B (E ())
change b = do  cur <- b
               when ((cur /=) <$> b)   
               
when :: B Bool -> B (E ())
when b = whenJust (boolToMaybe <$> b) 

boolToMaybe True   = Just ()
boolToMaybe False  = Nothing

snapshot :: B a -> E () -> B (E a)
snapshot b e =  let e' = (Just <$> b) <$ e
                in whenJust (pure Nothing `switch` e')

-- section 5, excluding EventStreams (which are in EventStream.hs)
-- section 5.1

countChanges :: Eq a => B a -> B (B Int)
countChanges b = loop 0 where
  loop :: Int -> B (B Int)
  loop i = do  e   <-  change b
               e'  <-  snapshot (loop (i+1)) e
               return (pure i `switch` e')

countChanges' :: Eq a => B a -> B (B Integer)
countChanges' = foldB (\x _ -> x + 1) (-1) 

foldB :: Eq a => (b -> a -> b) -> b -> B a -> B (B b)
foldB f i b = loop i where
  loop i = do  c   <- b
               let i' = f i c
               e   <-  change b
               e'  <-  snapshot (loop i') e
               return (pure i' `switch` e')

-- section 5.2

prev :: Eq a => a -> B a -> B (B a)
prev i b = (fst <$>) <$> foldB (\(_,p) c ->  (p,c)) (undefined,i) b

buffer :: Eq a => Int -> B a -> B (B [a])
buffer n b = foldB (\l e -> take n (e : l)) [] b

-- section 5.
plan :: E (B a) -> B (E a)
plan e =  whenJust 
           (pure Nothing `switch` ((Just <$>) <$> e))   


occ :: E a -> B (Maybe a)
occ e = pure Nothing `switch` ((pure . Just) <$> e) 


first :: E a -> E a -> B (E a)
first l r = whenJust (occ r `switch` ((pure . Just) <$> l)) 
