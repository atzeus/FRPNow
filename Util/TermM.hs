{-# LANGUAGE GADTs, Rank2Types #-}

module Util.TermM(TermM, Binds(..),prim,viewTermM) where

import Control.Applicative
import Control.Monad
import Debug.Trace

data TermM f a where
	Ret  :: a -> TermM f a
	Bnd  :: TermM f a -> (a -> TermM f b) -> TermM f b
	Prim :: f a -> TermM f a

instance Functor (TermM f) where
	fmap f m = pure f <*> m

instance Applicative (TermM f) where
	pure = return
	(<*>) = ap

instance Monad (TermM f) where
	return = Ret
	m >>= f  = Bnd m f

type Bind f a v = (forall w. f w -> (w -> TermM f a) -> v)

interpret :: Bind f a v -> (a -> v) -> TermM f a -> v
interpret bind ret = int where
  int (Ret a) = ret a
  int (Bnd (Prim x)  f) = bind x f
  int (Bnd (Ret x)   f) = int (f x)
  int (Bnd (Bnd p q) r) = int (Bnd p (\x -> Bnd (q x) r))

data Binds f a where
	Return  :: a -> Binds f a
	(:>>=)  :: f a -> (a -> TermM f b) -> Binds f b

prim :: f a -> TermM f a
prim p =  Bnd (Prim p) Ret

viewTermM (Ret a) = Return a
viewTermM (Prim p) = p :>>= return
viewTermM (Bnd (Ret a) f) = viewTermM (f a)
viewTermM (Bnd (Prim p) f) = p :>>= f
viewTermM (Bnd (Bnd m f) g) =  viewTermM (Bnd m (\x -> f x >>= g)) 
