{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, Rank2Types,RecursiveDo #-}

module TimeIVar(TimeStamp, TimeStamper, TIVar, withTimeStamper, endRound, newTIVar, writeTIVar,tvarAt,TIVarSupply,makeTIVar,runTIVarSupply ) where

import Control.Applicative hiding (empty)
import Control.Concurrent
import Control.Monad
import Control.Concurrent.MVar 
import System.IO.Unsafe
import Data.IORef
import Data.HashMap.Lazy hiding (map)
import Data.Hashable
import Data.Unique
import Data.Maybe


newtype TimeStamp   s   = TimeStamp Integer deriving (Eq,Ord)
newtype TimeStamper s   = TimeStamper (MVar Integer)
newtype TIVar       s a = TIVar (IORef (Maybe (Integer ,a)))


type Listeners s = HashMap Unique (TimeStamp s -> IO ())

instance Hashable Unique where
  hashWithSalt s a = hashUnique a + s

withTimeStamper :: (forall s. TimeStamper s -> IO a) -> IO a
withTimeStamper f = 
  do l <- newMVar 0
     f (TimeStamper l)

-- ends the current round, and gives back the 
-- the number of the round that just ended
-- Afterwards we can be sure that no more 
-- IVars get timestamp <= t 
endRound :: TimeStamper s -> IO (TimeStamp s)
endRound (TimeStamper l) = 
    do t <- takeMVar l
       putMVar l (t + 1)
       return (TimeStamp t)

newTIVar :: IO (TIVar s a)
newTIVar = liftM TIVar (newIORef Nothing) 

writeTIVar :: TimeStamper s -> TIVar s a -> a -> IO ()
writeTIVar (TimeStamper l) (TIVar r) a = 
  do t <- takeMVar l
     v <- readIORef r
     case v of
       Just _ -> error "Written to TIVar twice!"
       Nothing -> return ()
     writeIORef r (Just (t,a))
     putMVar l t 




tvarAt ::  TIVar s a -> TimeStamp s -> Maybe a
tvarAt s t = unsafePerformIO $ readTIVarAtIO t s


readTIVarAtIO :: TimeStamp s -> TIVar s a -> IO (Maybe a)
readTIVarAtIO (TimeStamp t) (TIVar r) = 
  do v <- readIORef r
     case v of
       Just  (t',a) | t' <= t -> return (Just a)
       _                      -> return Nothing
     
newtype TIVarSupply s a = TS {fromTS :: IO a } deriving Monad


makeTIVar :: TIVarSupply s (TIVar s a)
makeTIVar = TS $ newTIVar

runTIVarSupply :: TIVarSupply s a -> IO a
runTIVarSupply (TS m) = m

















{-
maybeGetTime :: TIVar s a -> IO (Maybe (TimeStamp s))
maybeGetTime (TIVar r) = 
  do rv <- readIORef r 
     case rv of
        Left l      -> return Nothing
        Right (t,a) -> return $ Just t


addListener :: TIVar s a -> (TimeStamp s -> IO ()) -> IO Unique
addListener (TIVar r) m = 
  do u <- newUnique
     atomicModIORef' r $ \(Left l) -> Left (insert u m l)
     return u

removeListener :: TIVar s a -> Unique -> IO ()
removeListener (TIVar r)  u = atomicModIORef' r remove
 where remove (Left l) = Left (delete u l)
       remove r        = r

atomicModIORef' r f =  atomicModifyIORef' r ((,()) . f)
-}

                                               























{-








instance Functor (TIVarSupply s) where
  fmap f = liftM f

instance Applicative (TIVarSupply s) where
  pure = return
  f <*> x = do fv <- f; xv <- x; return (fv xv)

-}


{-
data Time s = MinBound | Time (TimeStamp s) | MaxBound deriving (Eq, Ord)
newtype Event s a = AtStart a
                  | Ev (TIVar s a)

fromTimeStamp :: TimeStamp s -> Time s
fromTimeStamp = Time

evAt :: Event s a -> Time s -> Maybe a
evAt (AtStart x) _ = Just x
evAt (Event x)     = tivarAt x



instance Monad (Event s) where
  return = AtStart
  AtStart x >>= f = f x
  Ev m      >>= f = Ev $ unsafePerformIO $
     do res <- newTIVar
        withTime m $ \t -> 
         let x = f $ fromJust (m `evAt` t)
         in withTime x $ \t2 -> 
             unsafeSetTIVar res (max t t2) (fromJust $ x `evAt` t2)
        return res

withTime ::  TIVar s a -> (Time s -> IO ()) -> IO ()
withTime (AtStart x) f = f MinBound
withTime (Ev r) f = 
   do rv <- maybeGetTime r
      case rv of
        Just t    -> f t
        Nothing   -> void $ addListener r f


withEarliestTime :: TIVar s a -> TIVar s b -> (TimeStamp s -> IO ()) -> IO ()
withEarliestTime l r f = 
  do lv <- maybeGetTime l
     rv <- maybeGetTime r
     case (lv,rv) of
       (Just lt, Just rt) -> f (min lt rt)
       (Just lt, _      ) -> f lt
       (_      , Just rt) -> f rt
       _                  -> 
         mdo ul <- addListener l $ \t -> removeListener r ur >> f t
             ur <- addListener r $ \t -> removeListener l ul >> f t
             return ()



race :: TIVar s a -> TIVar s b -> TIVar s (Either a b)
race l r = TIVar $ unsafePerformIO $ 
  do res <- newIORef (Left empty)
     withEarliestTime l r $ \t -> unsafeSetTIVar (TIVar res) t (combine (l `at` t) (r `at` t)) 
     return res
 where combine _ (Just y) = Right y
       cobmine (Just x) _ = Left  x
                     

-}
