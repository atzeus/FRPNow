
{-# LANGUAGE NoMonomorphismRestriction, TypeOperators, MultiParamTypeClasses,FlexibleInstances #-}
-------------------------------------------------------------------------------------------
-- Some code stolen from :
-- Control.Functor.Composition  Copyright 	: 2008 Edward Kmett
-------------------------------------------------------------------------------------------

module FunctorCompose where
import Control.Monad
import Data.Monoid

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

class Flip a b where
  -- law (probably) : flip . fmap f = fmap f . flip
  flipF :: (a :. b) x -> (b :. a) x

flipDC = decomp . flipF . Comp

instance (Functor a, Functor c, Flip a c, Flip b c) => 
         Flip (a :. b) c  where
  flipF =  Comp . fmap Comp . decomp . flipF . Comp . fmap (decomp . flipF  . Comp) . decomp . decomp

instance (Flip e b, Monad e, Monad b) => Monad (b :. e) where
  return  = Comp . return . return
  m >>= f = Comp $ join $ liftM (liftM join . flipDC . liftM (decomp . f))  (decomp m) 
  
                
