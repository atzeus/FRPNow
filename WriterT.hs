module WriterT where

import Data.Monoid
import Prelude hiding (abs)
newtype DiffMonoid a = DM (a -> a) 

abs :: Monoid a => DiffMonoid a -> a
abs (DM f) = f mempty

rep :: Monoid a => a -> DiffMonoid a
rep a = DM (mappend a)

instance Monoid a => Monoid (DiffMonoid a) where 
  mempty = DM id
  mappend (DM f) (DM g) = DM ( f . g)

newtype WriterT w m a = WT (DiffMonoid w -> m (a, DiffMonoid w))

tell :: (Monad m, Monoid w) => w -> WriterT w m ()
tell w = WT (\w' -> return ((),w' `mappend` rep w))

runWriterT :: (Monad m, Monoid w) => WriterT w m a -> m (a, w)
runWriterT (WT f) = do (a,w) <- f mempty
                       return (a, abs w)

