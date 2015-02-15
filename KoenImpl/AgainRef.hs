module AgainRef(AgainRef, newAgainRef, readAgainRef) where


import Control.Monad.IO.Class

newtype AgainRef m x = AR { again :: x -> m x,
                            ref   :: IORef (m x) }

newAgainRef :: MonadIO m => (x -> m x) -> m x -> m (AgainRef m x)
newAgainRef f m = 
  do r <- newIORef m
     return (AR f r)

readAgainRef :: MonadIO m => AgainRef m x -> m x
readAgainRef (AR again r) = 
  do v <- readIORef r
     x <- v
     writeIORef r (again x)
     return x
  
