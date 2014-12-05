{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.FRPNow hiding (Behaviour, switch,whenJust, seqS, beforeSwitch, delay, getNow,cur)

infixr 3 :-> 
data BHT a = (:->) { headB :: a , tailB :: Event (Behaviour a) }

newtype Behaviour a = Behaviour { getNow :: Now (BHT a) }

instance Monad Behaviour where
  return a = Behaviour (return (a :-> never))
  m >>= f = memoB $
   do h :-> t <- getNow m
      getNow (f h `switch` fmap (>>= f) t)

instance MonadFix Behaviour where
  mfix f = memoB $ mfix (getNow . f . headB)

switch ::  Behaviour a -> Event (Behaviour a) -> Behaviour a
switch b e = Behaviour $ 
   evNow e >>= \case 
      Just (i,a) -> getNow a
      Nothing    -> 
        do h :-> t <- getNow b
           n <- raceObs t e
           return $ h :-> (either (`switch` e) id) <$> n


whenJust :: forall a. Behaviour (Maybe a) -> Behaviour (Event a)
whenJust b = memoB $ 
  do h :-> t <- getNow b
     let tw = fmap whenJust t
     case h of
      Just x  -> return (pure x :-> tw)
      Nothing -> do tn <- planIO (getNow <$> tw)
                    return $ (tn >>= headB) :-> (tn >>= tailB)


seqS :: Behaviour x -> Behaviour a -> Behaviour a
seqS l r = memoB $ 
  do (hl :-> sl) <- getNow l
     (hr :-> sr) <- getNow r
     return $ (hl `seq` hr) :-> ((l `seqS`) <$> sr)
     

getNowAgain :: BHT a -> Now (BHT a)
getNowAgain (h :-> t) = evNow t >>= \case 
      Just (_,b) -> getNow b
      Nothing    -> return (h :-> t)

data MemoInfo a = MemoInfo { time :: Time, cur :: BHT a } 

memoB :: Now (BHT a) -> Behaviour a
memoB m = Behaviour $ runMemo $ unsafePerformIO $ (,) <$> newMVar True <*> newEmptyMVar 
 where runMemo (first,memo) = 
         do isFirst <- syncIO $ takeMVar first
            if isFirst 
            then do v <- m
                    t <- getRound 
                    syncIO $ putMVar memo (MemoInfo t v)
                    syncIO $ putMVar first False
                    return v
             else memoBVar memo

memoBVar :: MVar (MemoInfo a) -> Now (BHT a)
memoBVar m = do memo <- syncIO $ takeMVar m
                t <- getRound 
                res <- getNowAgain (cur memo)
                syncIO $ putMVar m (MemoInfo t res)
                return res

