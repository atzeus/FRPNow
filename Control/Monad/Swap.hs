
{-# LANGUAGE ConstraintKinds, FlexibleContexts, InstanceSigs,  NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses,FlexibleInstances #-}

module Control.Monad.Swap where
import Control.Monad
import Control.Applicative

newtype (f :. g) x = Comp (f (g x))

open (Comp x) = x
close = Comp

assoc :: Functor f => ((f :. g) :. h) x -> (f :. (g :. h)) x
assoc = close . fmap close . open . open

coassoc :: Functor f => (f :. (g :. h)) x -> ((f :. g) :. h) x
coassoc = close . close . fmap open . open

instance (Functor a, Functor b) => Functor (a :. b) where 
  fmap f = close . fmap (fmap f) . open

class Swap f g where
  -- laws (from Composing Monads, Jones and Duponcheel)
  -- swap . fmap (fmap f) = fmap (fmap f) . swap
  -- swap . return        = fmap unit
  -- swap . fmap return   = return
  -- prod . fmap dorp     = dorp . prod 
  --             where prod = fmap join . swap
  --                   dorp = join . fmap swap
  --             
  swap :: f (g a) -> g (f a)

instance Swap a a where
  swap = id

-- actually only requirement on g is pointer functor and f functor
liftLeft :: (Monad f, Monad g) => f x -> (f :. g) x 
liftLeft = close . liftM return 

-- actually only requirement on f is pointer functor 
liftRight :: Monad f => g x -> (f :. g) x 
liftRight  = close . return 


instance (Functor a, Functor c, Swap a c, Swap b c) => 
         Swap (a :. b) c  where
  swap =   fmap close . swap . fmap swap . open 

instance (Functor a, Functor b, Swap a b, Swap a c) => 
      Swap a (b :. c)  where
  swap =  close . fmap swap . swap . fmap open 

instance (Swap g f, Monad f, Monad g) => Monad (f :. g) where
  -- see (Composing Monads, Jones and Duponcheel) for proof
  return  = close . return . return
  m >>= f = joinComp (fmap2m f m)

-- anoyance that Monad is not a subclass of functor
fmap2m f = close . liftM (liftM f) . open

joinComp :: (Swap e b, Monad e, Monad b) => (b :. e) ((b :. e) x) -> (b :. e) x
joinComp = close . joinFlip . open . fmap2m open

joinFlip :: (Swap e b, Monad e, Monad b) => b (e (b (e x))) -> b (e x)
joinFlip =  liftM join . join . liftM swap 
-- this works as follows, we have 
-- b . e . b . e      flip middle two
-- b . b . e . e      join left and right
-- b . e 


instance (Applicative b, Applicative e) => Applicative (b :. e) where
   pure = close . pure . pure
   x <*> y = close $ (<*>) <$> open x <*> open y  
