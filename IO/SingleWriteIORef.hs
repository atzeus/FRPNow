module IO.SingleWriteIORef(SIORef, newSIORef, readSIORef, putSIORef) where

import Data.IORef
import Data.Unique

newtype SIORef a = SIO (IORef (Maybe a), Unique)

instance Show (SIORef a) where
  show (SIO (_,u)) = show (hashUnique u)

newSIORef :: IO (SIORef a)
newSIORef = do d <- newUnique
               r <- newIORef Nothing
              
               return (SIO (r,d))

readSIORef :: SIORef a -> IO (Maybe a)
readSIORef (SIO (r,_)) = readIORef r 

putSIORef :: SIORef a -> a -> IO ()
putSIORef (SIO (r,u)) a = 
  do rv <- readIORef r
     case rv of
        Just _ -> error "Already wrote to single write IORef"
        Nothing -> writeIORef r (Just a)

