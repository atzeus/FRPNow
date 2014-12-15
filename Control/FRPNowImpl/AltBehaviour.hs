
{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Control.FRPNowImpl.Behaviour where
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.FRPNowImpl.Event
import Debug.Trace

infixr 3 :-> 
data BHT a = (:->) { headB :: a , tailB :: Event (Behaviour a) }

newtype Behaviour a = Behaviour { getHT :: Now (BHT a) }

curIO :: Behaviour a -> Now a
curIO b = headB <$> getHT b

instance Monad Behaviour where
  return a = Behaviour (return (a :-> never))
  m >>= f = memo (bind m f)

bind m f =  Behaviour $
   do h :-> t <- getHT m
      getHT (f h `switch` fmap (`bind` f) t)


instance MonadFix Behaviour where
  mfix f = memo $ Behaviour $ mfix (getHT . f . headB )


switch ::  Behaviour a -> Event (Behaviour a) -> Behaviour a
switch b e = Behaviour  $ 
   evNow e >>= \case 
      Just (_,a) -> trace "Switched!" $ getHT a
      Nothing    -> 
        do h :-> t <- getHT b
           n <- raceObs t e
           return $ h :-> (either (`switch` e) id) <$> n

whenJust b = memo (whenJust' b)

whenJust' :: forall a. Behaviour (Maybe a) -> Behaviour (Event a)
whenJust' b = Behaviour $ 
  do h :-> t <- getHT b
     let tw = fmap whenJust' t
     case h of
      Just x  -> return (pure x :-> tw)
      Nothing -> do tn <- planIO (getHT <$> tw)
                    return $ (tn >>= headB) :-> (tn >>= tailB)

seqS l r = memo (seqS' l r)

seqS' :: Behaviour x -> Behaviour a -> Behaviour a
seqS' l r = Behaviour $ 
  do (hl :-> sl) <- getHT l
     (hr :-> sr) <- getHT r
     return $ (hl `seq` hr) :-> ((l `seqS'`) <$> sr)



getNowAgain :: BHT a -> Now (BHT a)
getNowAgain (h :-> t) = evNow t >>= \case 
      Just (_,b) -> getHT b
      Nothing    -> return (h :-> t)


memo :: Behaviour a -> Behaviour a
memo b = Behaviour $ unsafePerformIO $ runMemo <$> newMVar (Left b) where
  runMemo m = 
     do v <- syncIO $ readMVar m 
        res <- case v of
                Left b -> getHT b 
                Right m -> getNowAgain m
        syncIO $ swapMVar m (Right res)
        return res
{- NOINLINE memo -}               



instance Functor Behaviour where
  fmap = liftM

instance Applicative Behaviour where
  pure = return
  (<*>) = ap

{-
elimSwitch :: Behaviour a -> Behaviour a
elimSwitch n = Behaviour $ unsafePerformIO $ runMemo <$> newMVar (Left n) where
  runMemo m = 
    do v <- trace "Take" $ syncIO (takeMVar m) 
       (v',res) <- case v of
               Right n -> return (Right n, Switched (again m n))
               Left  n -> getHT' n >>= return . \case 
                            Switched n' -> (Right n', Switched (again m n'))
                            nf          -> (Left n  , nf)
       syncIO $ putMVar m v'
       return res

  again m n = Behaviour $ getHT' n >>= \case 
     Switched n' -> trace "huh" (syncIO (swapMVar m (Right n'))) >> getHT' (again m n')
     nf -> return nf
{- NOINLINE elimSwitch -}
-}
