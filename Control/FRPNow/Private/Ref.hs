module Control.FRPNow.Private.Ref where

import System.Mem.Weak
import Control.Applicative
import Data.IORef
import Debug.Trace

data Ref a = W (Weak a)
           | S a


makeWeakIORef :: IORef a -> IO (Ref (IORef a))
makeWeakIORef v = W <$> mkWeakIORef v (return ())
{-
makeWeakRef ::  k -> v ->  IO (Ref v)
makeWeakRef  k v = W <$> mkWeak k v Nothing
-}
makeStrongRef :: v -> IO (Ref v)
makeStrongRef v = return $ S v


deRef :: Ref a -> IO (Maybe a)
deRef (S a) = return (Just a)
deRef (W a) = deRefWeak a
