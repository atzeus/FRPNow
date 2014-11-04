module Behaviour where

import Event

import Control.Applicative
import Control.Monad.Fix
import Control.Monad


data Steps s a = Step { getHead :: a, getTail :: Event s (Steps s a) }

switchS :: Steps s a -> Event s (Steps s a) -> Steps s a
(h `Step` t) `switchS` e = h `Step` (fmap nxt $ race t e) where
  nxt (Left b)  = b `switchS` e
  nxt (Right b) = b

whenJustS :: Steps s (Maybe a) -> Steps s (Event s a)
whenJustS (Just x  `Step` e) = pure x `Step` fmap whenJustS e
whenJustS (Nothing `Step` e) = let e' = fmap whenJustS e
                               in  (e' >>= getHead) `Step` (e' >>= getTail)
   
data Behaviour s a = B { stepsFrom :: Time s -> Steps s a }

instance Monad (Behaviour s) where
  return x = B $ const (x `Step` never)
  (B m) >>= f = B $ \t -> stepsFrom (f (getHead (m t))) t

instance MonadFix (Behaviour s) where
  mfix f = B $ \t -> let b = stepsFrom (f (getHead b)) t
                     in b

evToSteps :: Event s (Behaviour s a) -> Event s (Steps s a)
evToSteps e = memoEv $ \t -> 
     case e `getAt` t of
      Just (t',b) -> Just (t', stepsFrom b t')
      Nothing -> Nothing
      
forgetPast :: Time s -> Steps s a -> Steps s a
forgetPast t = loop where
  loop (a `Step` b) = case b `getAt` t of
      Just (_,b) -> loop b
      Nothing    -> a `Step` b
      
switch :: Behaviour s a -> Event s (Behaviour s a) -> Behaviour s a
switch (B f) e = B $ \t -> case e `getAt` t of
   Just (_,b) -> stepsFrom b t
   Nothing    -> f t `switchS` fmap evToSteps e

whenJust :: Behaviour s (Maybe a) -> Behaviour s (Event s a)
whenJust (B f) = B $ \t -> fmap (delay t) (whenJustS (f t)) --  + delay

instance Functor (Behaviour s) where
  fmap = liftM

instance Applicative (Behaviour s) where
  pure = return
  (<*>) = ap


{-

-- these discrete semantics correspond to the actual semantics by
-- the following function:

-- [.] = toDenotation
-- [x `Step` (te,y)] = \t -> if t < te then x else [y] t
instance Monad (Behaviour s) where
  return x = x `Step` never
  (h `Step` t) >>= f = f h `switch` fmap (>>= f) t


instance MonadFix (Behaviour s) where
  mfix f = let b@(a `Step` t) = f a in b 

-- That the above corresponds to the 
-- semantics is not so obvious (to me)
--
-- To prove:
-- [mfix f] = [mfix] (\a -> [f a])

-- Proof by rewriting
-- lhs : [let b@(a `Step` t) = f a in b]                                  -- unfold of mfix
--       let b@(a `Step` t) = f a in [b]                                  -- push [.] inwards
--       let (a `Step` (te,y)) = f a in \t -> if t < te then a else [y] t -- unfold [.]
--       \t -> let (a `Step` (te,y)) = f a in if t < te then a else [y] t -- lift lambda
--
-- rhs : (\f t -> let a = f a t in a) (\a -> [f a]) -- unfold mfix(denot)
--       \t -> let a = [f a] t in a                  -- inline arg
--       \t -> let a = let (a `Step` (te,y) in if t < te then x else [y] t) in a in a -- unfold [.]
--       \t -> let (a `Step` (te,y)) = f a in if t < te then a else [y] t  -- rewrite lets


-}
