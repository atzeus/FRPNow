
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, LambdaCase #-}
module Control.FRPNowImpl.Behaviour(Behaviour,curIO, switch, whenJust) where
import Control.Applicative hiding (empty,Const)
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.FRPNowImpl.NowEvent
import Data.Sequence
import Data.Foldable (toList)
import Data.Maybe

infixr 3 :-> 
data BHT s a = (:->) { headB :: Now s a , tailB :: Event s (Behaviour s a) }
            | SameAs (Behaviour s a) (BHT s a) 
            | Const a
   


newtype Behaviour s a = B { getHT :: Now s (BHT s a) }

              
curIO :: Behaviour s a -> Now s a
curIO b = getHT b >>= headB . normHTFull 

instance Monad (Behaviour s) where
  return a = B $ return (Const a)
  m >>= f = memo $ bind m f


bind :: Behaviour s a -> (a -> Behaviour s b) -> Behaviour s b
bind m f = B $
   do v <- noSameAs . normalizeBNF <$> getHT m
      case v of
       h :-> t -> do x <- h
                     let t' = (`bind` f) <$> t
                     getHT (f x `switch'` t')
       Const x -> let v = f x 
                  in SameAs v <$> getHT v

switch b e =  memo $  switch' b e

switch' ::  Behaviour s a -> Event s (Behaviour s a) -> Behaviour s a
switch' b e = B  $ 
  evNow e >>= \case 
    Just a  -> SameAs a <$> getHT a
    Nothing -> normHT <$> getHT b >>= \case
        h :-> t -> do let t' = (`switch'` e) <$> t
                      ts <- t' `firstObs` e
                      return $ h :-> ts
        Const x -> return (pure x :-> e)

whenJust b =  whenJust' b




whenJust' :: Behaviour s (Maybe a) -> Behaviour s (Event s a)
whenJust' b = B $ 
  normHT <$> getHT b >>= \case
      Const x -> return $ Const (maybe never return x)
      h :-> t -> do let tw = whenJust' <$> t
                    h' <- constMemo (head h tw)
                    return (h' :-> tw)
  where head h tw = h >>= \case 
         Just x  -> return $ pure x
         Nothing -> join <$> planIO (curIO  <$> tw)

{-

seqS :: Behaviour x -> Behaviour a -> Behaviour a
seqS l r = B $ 
  do (hl :-> sl) <- getHT l
     (hr :-> sr) <- getHT r
     h  <- constMemo $ hl >> hr
     let t = (l `seqS`) <$> sr
     return $ h :-> t
      
-}



getNowAgain :: BHT s a -> Now s (BHT s a)
getNowAgain (h :-> t) = evNow t >>= \case
      Just x  -> getHT x >>= getNowAgain
      Nothing -> return (h :-> t)
getNowAgain (SameAs _ b) = getNowAgain b
getNowAgain (Const x)    = return $ Const x



constMemo :: Now s a -> Now s (Now s a)
constMemo n = syncIO $ runMemo <$> newMVar (Left n) where
  runMemo m = do v <- syncIO $ takeMVar m
                 v' <- case v of
                         Left x  -> x 
                         Right x -> return x
                 syncIO $ putMVar m (Right v')
                 return v'


data MemoInfo s a = Uninit (Behaviour s a) | Init (BHT s a) | SameAsS (Behaviour s a) | ConstS a

{-
memo' :: Behaviour s a -> Behaviour s a
memo' b = B $ unsafePerformIO $ runMemo <$> newMVar False where
  runMemo m = 
     do v <- syncIO $ takeMVar m 
        if v 
         then syncIO $ putStrLn "Hallo!"
         else return ()
        res <- getHT b      
        syncIO $ putMVar m False
        return res
{-# NOINLINE memo' #-}       
-}


memo :: Behaviour s a -> Behaviour s a
memo b = B $ unsafePerformIO $ runMemo <$> newMVar (Uninit b) where
  runMemo m = 
    do  v <- syncIO $ takeMVar m 
        res <- case v of
                Uninit b  -> getHT b 
                Init m    -> getNowAgain m
                SameAsS b -> SameAs b <$> getHT b
                ConstS x  -> return (Const x)
        let (newState, res') = case normalizeBNF res of
                                SameAs n nf -> (SameAsS n, SameAs n nf)
                                Const x     -> (ConstS x, SameAs (return x) (Const x))
                                nf          -> (Init nf, nf)
        syncIO $ putMVar m newState
        return res'
{-# NOINLINE memo #-}    

   
noSameAs (SameAs _ (SameAs _ _)) = error "Double same as!"
noSameAs (SameAs _ nf) = nf
noSameAs n             = n

normHTFull nf = case noSameAs (normalizeBNF nf) of
             Const x -> return x :-> never
             x       -> x

normHT nf = noSameAs (normalizeBNF nf)

normalizeBNF (SameAs _ (SameAs n nf)) =  normalizeBNF $ SameAs n nf
normalizeBNF (SameAs _ (Const x))     =  Const x
normalizeBNF nf              = nf

instance Functor (Behaviour s) where
  fmap = liftM

instance Applicative (Behaviour s) where
  pure = return
  (<*>) = ap




