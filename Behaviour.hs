module Behaviour where

import Event

import Control.Applicative
import Control.Monad.Fix

-- these discrete semantics correspond to the actual semantics by
-- the following function:

-- [.] = toDenotation
-- [x `Step` (te,y)] = \t -> if t < te then x else [y] t

data Behaviour a = Step { getHead :: a, getTail :: Event (Behaviour a) }

instance Monad Behaviour where
  return x = x `Step` never
  (h `Step` t) >>= f = f h `switch` fmap (>>= f) t


instance MonadFix Behaviour where
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




(h `Step` t) `switch` e = h `Step` (fmap nxt $ race t e) where
  nxt (Left b)  = b `switch` e
  nxt (Right b) = b

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust (Just x  `Step` e) = pure x `Step` fmap whenJust e
whenJust (Nothing `Step` e) = let e' = fmap whenJust e
                              in  (e' >>= getHead) `Step` (e' >>= getTail)



-- external interface..,
sample :: Event () -> Behaviour a -> Event a
sample e = loop where
  loop b@(h `Step` t) = 
     do v <- race t e
        case v of
           Left b'  -> loop b'
           Right () -> return h




instance Functor Behaviour where
  fmap f a = a >>= return . f

instance Applicative Behaviour where
  pure = return
  f <*> g = do x <- f ; y <- g ; return (x y)
