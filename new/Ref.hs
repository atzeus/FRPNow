module Ref where

import System.Mem.Weak
import Control.Applicative

data Ref a = W (Weak a)
           | S a

makeWeakRefKey :: k -> a -> IO (Ref a)
makeWeakRefKey k a = W <$> mkWeak k a Nothing

makeWeakRef :: a -> IO (Ref a)
makeWeakRef a = W <$> mkWeakPtr a Nothing

makeStrongRef :: Monad m => a -> m (Ref a)
makeStrongRef a = return (S a)

isWeak (W _) = True
isWeak _     = False

deRef :: Ref a -> IO (Maybe a)
deRef (S a) = return (Just a)
deRef (W a) = deRefWeak a
