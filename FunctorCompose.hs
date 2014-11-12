
{-# LANGUAGE FlexibleContexts, InstanceSigs,  NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses,FlexibleInstances #-}
-------------------------------------------------------------------------------------------
-- Some code stolen from :
-- Control.Functor.Composition  Copyright 	: 2008 Edward Kmett
-------------------------------------------------------------------------------------------

module FunctorCompose where
import Control.Monad
import Data.Monoid
import Control.Applicative

newtype (f :. g) x = Comp {decomp :: (f (g x)) }

data Id a = Id {fromId :: a}

norml :: Functor a => (Id :. a) x -> a x
norml (Comp x) = fromId x

normr :: Functor a => (a :. Id) x -> a x
normr (Comp x) = fmap fromId x

assoc :: Functor f => ((f :. g) :. h) x -> (f :. (g :. h)) x
assoc = Comp . fmap Comp . decomp . decomp

coassoc :: Functor f => (f :. (g :. h)) x -> ((f :. g) :. h) x
coassoc = Comp . Comp . fmap decomp . decomp

instance (Functor a, Functor b) => Functor (a :. b) where 
  fmap f = Comp . fmap (fmap f) . decomp

class FlipF a b where
  -- law (probably) : flip . fmap f = fmap f . flip
  flipF :: a (b x) -> b (a x)

instance FlipF a a where
  flipF = id

liftFL :: (Functor f, Monad m) => f x -> (f :. m) x 
liftFL = Comp . fmap return 

liftFR :: Monad f => g x -> (f :. g) x 
liftFR  = Comp . return 

flipDC = decomp . flipF . Comp

instance (Functor a, Functor c, FlipF a c, FlipF b c) => 
         FlipF (a :. b) c  where
  flipF =   fmap Comp . flipF . fmap flipF . decomp 

instance (Functor a, Functor b, FlipF a b, FlipF a c) => 
      FlipF a (b :. c)  where
  flipF =  Comp . fmap flipF . flipF . fmap decomp 

instance (Applicative b, Applicative e) => 
    Applicative (b :. e) where
   pure x = Comp (pure (pure x))
   (Comp x) <*> (Comp y) = Comp $ (<*>) <$> x <*> y
instance (FlipF e b, Monad e, Monad b) => Monad (b :. e) where
  return  = Comp . return . return
  m >>= f = joinFlip (fmap2m f m)

-- annoyance that monad is not subclass of functor
fmap2m :: (Monad e, Monad b) => (x -> y) -> (b :. e) x -> (b :. e) y
fmap2m f = Comp . liftM (liftM f) . decomp 

joinFlip :: (FlipF e b, Monad e, Monad b) => ((b :. e) ( (b :. e) x )) -> (b :. e) x 
joinFlip =  Comp . join . liftM (liftM join . flipF . liftM (decomp)) . decomp                
-- this works as follows, we have 
-- b . e . b . e      flip middle two
-- b . b . e . e      join left and right
-- b . e 




