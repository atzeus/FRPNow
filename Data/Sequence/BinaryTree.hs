{-# LANGUAGE Rank2Types,GADTs, DataKinds, TypeOperators #-}



-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Sequence.BinaryTree
-- Copyright   :  (c) Atze van der Ploeg 2014
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- A sequence, implemented as a binary tree, good performance when used emperally
--
--
-----------------------------------------------------------------------------
module Data.Sequence.BinaryTree(module Data.SequenceClass,Queue)  where
import Control.Applicative (pure, (<*>), (<$>))
import Data.Foldable
import Data.Monoid ((<>))
import Data.Traversable
import Prelude hiding (foldr,foldl)
import Data.SequenceClass

data BSeq

data P a = a :* a 

instance Functor P where
  fmap f (a :* b) = f a :* f b

instance Foldable P where
  foldl f z (a :* b) = f (f z a) b
  foldr f z (a :* b) = f a (f b z)
  foldMap f (a :* b) = f a <> f b

instance Traversable P where
  traverse f (a :* b) = (:*) <$> f a <*> f b

data B a where
  B1 :: a    -> B a
  B2 :: !(P a)  -> B a

instance Functor B where
 fmap phi (B1 c) = B1 (phi c)
 fmap phi (B2 p) = B2 (fmap phi p)

instance Foldable B where
  foldl f z (B1 x) = f z x
  foldl f z (B2 p) = foldl f z p
  foldr f z (B1 x) = f x z
  foldr f z (B2 p) = foldr f z p
  foldMap f (B1 x) = f x
  foldMap f (B2 p) = foldMap f p

instance Traversable B where
  traverse f (B1 x) = B1 <$> f x
  traverse f (B2 p) = B2 <$> traverse f p

data Queue a  where
  Q0 :: Queue a 
  Q1 :: a  -> Queue a
  QN :: !(B a) -> Queue (P a) -> !(B a) -> Queue a

instance Functor Queue where
  fmap f Q0 = Q0
  fmap f (Q1 x) = Q1 (f x)
  fmap f (QN l m r) = QN (fmap f l) (fmap (fmap f) m) (fmap f r)

instance Foldable Queue where
  foldl f = loop where
    loop i s = case viewl s of
          EmptyL -> i
          h :< t -> loop (f i h) t
  foldr f i s = foldr f i (reverse $ toRevList s)
    where toRevList s = case viewl s of
           EmptyL -> []
           h :< t -> h : toRevList t

instance Traversable Queue where
  traverse f Q0 = pure Q0
  traverse f (Q1 x) = Q1 <$> f x
  traverse f (QN b1 q b2) = QN <$> traverse f b1 <*> traverse (traverse f) q <*> traverse f b2

instance Sequence Queue where
  empty = Q0
  singleton = Q1 
  q |> b = case q of
    Q0             -> Q1 b
    Q1 a           -> QN (B1 a) Q0 (B1 b)
    QN l m (B1 a)  -> QN l m (B2 (a :* b)) 
    QN l m (B2 r)  -> QN l (m |> r) (B1 b)

  viewl q = case q of
    Q0                    -> EmptyL
    Q1 a                  -> a :< Q0
    QN (B2 (a :* b)) m r  -> a :< QN (B1 b) m r
    QN (B1 a) m r         -> a :< shiftLeft m r
    where  
           shiftLeft q r = case viewl q of
               EmptyL -> buf2queue r
               l :< m -> QN (B2 l) m r
           buf2queue (B1 a)        = Q1 a
           buf2queue(B2 (a :* b))  = QN (B1 a) Q0 (B1 b)



