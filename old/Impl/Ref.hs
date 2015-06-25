module Impl.Ref where

import System.Mem.Weak
import Control.Applicative
import Data.IORef

data Ref a = W {-# UNPACK #-}  !(Weak a)
           | S a

makeWeakIORef :: IORef a -> IO (Ref (IORef a))
makeWeakIORef r = W <$> mkWeakIORef r (return ())
{-# INLINE makeWeakIORef #-}

makeWeakRefKey :: k -> a -> IO (Ref a)
makeWeakRefKey k a = W <$> mkWeak k a Nothing
{-# INLINE makeWeakRefKey #-}

makeWeakRef :: a -> IO (Ref a)
makeWeakRef a = W <$> mkWeakPtr a Nothing
{-# INLINE makeWeakRef #-}

makeStrongRefKey ::  Monad m => k -> a -> m (Ref a)
makeStrongRefKey k a = return (S a)
{-# INLINE makeStrongRefKey #-}

makeStrongRef :: Monad m => a -> m (Ref a)
makeStrongRef a = return (S a)
{-# INLINE makeStrongRef #-}

isWeak (W _) = True
isWeak _     = False
{-# INLINE isWeak #-}

deRef :: Ref a -> IO (Maybe a)
deRef (S a) = return (Just a)
deRef (W a) = deRefWeak a
{-# INLINE deRef #-}
