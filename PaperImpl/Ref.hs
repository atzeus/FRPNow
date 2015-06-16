module Ref where

import System.Mem.Weak
import Control.Applicative
import Data.IORef

data Ref a = W (Weak a)
           | S a

makeWeakRef :: a -> IO (Ref a)
makeWeakRef v = W <$> mkWeakPtr v Nothing

makeStrongRef :: a -> IO (Ref a)
makeStrongRef v = return $ S v


deRef :: Ref a -> IO (Maybe a)
deRef (S a) = return (Just a)
deRef (W a) = deRefWeak a
