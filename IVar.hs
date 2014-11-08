module IVar where

import Data.IORef
import Control.Monad

newtype IVar a = IVar (IORef (Maybe a))

newIVar :: IO (IVar a)
newIVar = liftM IVar (newIORef Nothing)

writeIVar :: IVar a -> a -> IO ()
writeIVar (IVar r) a = 
  do v <- readIORef r
     case v of
      Just x -> error "Written to IVar twice!"
      Nothing -> writeIORef r (Just a)

readIVar :: IVar a -> IO (Maybe a)
readIVar (IVar r) = readIORef r
