{-# LANGUAGE GADTs #-}

module TermM(TermM, Binds(..),viewTermM) where

data TermM f a where
	Ret  :: a -> TermM f a
	Bnd  :: TermM f a -> (a -> TermM f b) -> TermM f b
	Prim :: f a -> TermM f a

instance Monad (TermM f) where
	return = Ret
	(>>=)  = Bnd

data Binds f a where
	Return  :: a -> Binds f a
	(:>>=)  :: f a -> (a -> TermM f b) -> Binds f b

viewTermM (Ret a) = Return a
viewTermM (Bnd (Prim p) f) = p :>>= f
viewTermM (Bnd (Bnd m f) g) = viewTermM (Bnd m (\x -> f x >>= g)) 