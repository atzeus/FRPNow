{-# LANGUAGE LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}

module Syntactic.Behaviour where

import Data.Sequence
import Control.Applicative hiding (empty,Const)
import Syntactic.Time
import Data.Foldable
import Control.Monad
import System.IO.Unsafe

data Behaviour a where
 Const      :: a -> Behaviour a
 Switch     :: Behaviour a -> Event (Behaviour a) -> Behaviour a
 Bnd        :: Behaviour a -> (a -> Behaviour b) -> Behaviour b
 WhenJust   :: Behaviour (Maybe a) -> Behaviour (Event a)
 SeqB       :: Behaviour x -> Behaviour a -> Behaviour a

instance Monad Behaviour where
  return = Const
  (>>=)  = Bnd

instance Functor Behaviour where
  fmap = liftM

instance Applicative Behaviour where
  pure = return
  (<*>) = ap

switch = Switch
whenJust = WhenJust

type BehaviourNF a = (a, Seq (Event (Behaviour a)))

cur :: Behaviour a -> Present a
cur m = fst <$> toNormalForm m

toNormalForm :: Behaviour a -> Present (BehaviourNF a)
toNormalForm e = case e of
  Const x -> return (x,empty)

  Switch b e -> evPresent e >>= \case 
                     Just x  -> toNormalForm x
                     Nothing -> (\(x,l) -> (x, e <| l)) <$> toNormalForm b

  Bnd b f -> do (a,sa) <- toNormalForm b 
                (b,sb) <- a `seq` toNormalForm (f a)
                let sa' = fmap (fmap (`Bnd` f)) sa
                return (b,sa' >< sb) 

  WhenJust b -> do (a,s) <- toNormalForm b
                   let s' = fmap (fmap WhenJust) s
                   ev <- case a of
                     Just x  -> return $ return x
                     Nothing -> join <$> planFirst (fmap (fmap cur) (toList s'))
                   return (ev,s')

  SeqB a b ->  do (av,_ ) <- toNormalForm a 
                  (bv,sb) <- toNormalForm b
                  return (av `seq` bv, fmap (fmap (SeqB a)) sb)

{-
data BehaviourSyntax a where
 Const    :: a -> BehaviourSyntax a
 Switch   :: Behaviour a -> Event (Behaviour a) -> BehaviourSyntax a
 Bnd      :: Behaviour a -> (a -> Behaviour b) -> BehaviourSyntax b
 SameAs   :: Behaviour a -> BehaviourSyntax a
 WhenJust :: Behaviour (Maybe a) -> BehaviourSyntax (Event a)
 SeqB     :: Behaviour x -> Behaviour a -> BehaviourSyntax a


newtype Behaviour a = B (MVar (RoundNr, Either (BehaviourSyntax a) (BehaviourNF a)))

newBehaviour :: BehaviourSyntax a -> Behaviour a
newBehaviour s = unsafePerformIO $ B <$> newMVar (-1, Left s)
{-# NOINLINE newBehaviour #-}

switch b e = newBehaviour (Switch b e)
whenJust b = newBehaviour (WhenJust b)
seqB x b   = newBehaviour (SeqB x b)

instance Monad Behaviour where
  return  = newBehaviour . Const
  m >>= f = newBehaviour (Bnd m f)


data BehaviourNF a = SameAs (Behaviour a)
                   | Step a (Seq (Event (Behaviour a)))

toNormalForm :: Behaviour a -> Present ()
toNormalForm (B m) = do (i,s) <- syncDo $ takeMVar m
                        j     <- getRound
                        if i == j 
                        then case s of
                              Right nf -> putMVar m (i,s) >> return nf
                        else case s of
                              Right nf -> error "need to think about this"
                              Left s   -> tnf s
 tnf e = case e of
  Const x -> return (x `Step` empty)

  Switch b e -> evPresent e >>= \case 
                     Just x  -> toNormalForm x >> return (SameAs x)
                     Nothing -> (\(x,l) -> (x, e <| l)) <$> toNormalForm b

  Bnd b f -> do (a,sa) <- toNormalForm b 
                (b,sb) <- a `seq` toNormalForm (f a)
                let sa' = fmap (fmap (`Bnd` f)) sa
                return (b,sa' >< sb) 

  WhenJust b -> do (a,s) <- toNormalForm b
                   let s' = fmap (fmap WhenJust) s
                   ev <- case a of
                     Just x  -> return $ return x
                     Nothing -> join <$> planFirst (fmap (fmap cur) (toList s'))
                   return (ev,s')

  SeqB a b ->  do (av,_ ) <- toNormalForm a 
                  (bv,sb) <- toNormalForm b
                  return (av `seq` bv, fmap (fmap (SeqB a)) sb)



instance Functor Behaviour where
  fmap = liftM

instance Applicative Behaviour where
  pure = return
  (<*>) = ap
-}

