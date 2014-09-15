module IO.SingleWriteIORef(SIORef, newSIORef, readSIORef, putSIORef) where

import Data.IORef

newtype SIORef a = SIO (IORef (Maybe a))

newSIORef :: IO (SIORef a)
newSIORef = do r <- newIORef Nothing
               return (SIO r)

readSIORef :: SIORef a -> IO (Maybe a)
readSIORef (SIO r) = readIORef r

putSIORef :: SIORef a -> a -> IO ()
putSIORef (SIO r) a = 
  do rv <- readIORef r
     case rv of
        Just _ -> error "Already wrote to single write IORef"
        Nothing -> writeIORef r (Just a)

