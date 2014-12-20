
{-# LANGUAGE RecursiveDo, ScopedTypeVariables, LambdaCase #-}
module Control.FRPNowImpl.Behaviour(Behaviour,curIO, switch, whenJust) where
import Control.Applicative hiding (empty,Const)
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.FRPNowImpl.Event
import Control.FRPNowImpl.Now
import Data.Sequence
import Data.Foldable (toList)
import Data.Maybe

infixr 3 :-> 
data BHT s a = (:->) { headB :: Now s a , tailB :: Event s (Behaviour s a) }
            | Const a

newtype Behaviour s a = B { getHT :: Now s (BHT s a) }

again :: BHT s a -> Behaviour s a
again x = B $ 
  case x of
    Const _ -> return x
    h :-> t -> evNow t >>= \case 
                Just b ->  getHT b 
                Nothing -> return x
             
curIO :: Behaviour s a -> Now s a
curIO b = getHT b >>= \case
             h :-> t -> h
             Const x -> return x

instance Monad (Behaviour s) where
  return a = B $ return (Const a)
  m >>= f = memo $ bind m f


bind :: Behaviour s a -> (a -> Behaviour s b) -> Behaviour s b
bind m f = B $
   do v <- getHT m
      case v of
       h :-> t -> do x <- h
                     let t' = (`bind` f) <$> t
                     getHT (f x `switch` t')
       Const x -> let v = f x 
                  in getHT v


switch ::  Behaviour s a -> Event s (Behaviour s a) -> Behaviour s a
switch b e = B  $ 
  evNow e >>= \case 
    Just a  -> getHT a
    Nothing -> getHT b >>= \case
        h :-> t -> do let t' = (`switch` e) <$> t
                      ts <- t' `firstObs` e
                      return $ h :-> ts
        Const x -> return (pure x :-> e)

whenJust b =  whenJust' b


whenJust' :: Behaviour s (Maybe a) -> Behaviour s (Event s a)
whenJust' b = B $ 
  getHT b >>= \case
      Const x -> return $ Const (maybe never return x)
      h :-> t -> do let tw = whenJust' <$> t
                    h' <- constMemo (head h tw)
                    return (h' :-> tw)
  where head h tw = h >>= \case 
         Just x  -> return $ pure x
         Nothing -> join <$> planIO (curIO  <$> tw)


memo :: Behaviour s a -> Behaviour s a
memo b = B $ runMemo  where
  mvar = unsafePerformIO $ newMVar b
  {-# NOINLINE mvar #-}  
  runMemo = 
    do b <- syncIO $ takeMVar mvar 
       bht <- getHT b
       syncIO $ putMVar mvar (again bht)
       return bht


constMemo :: Now s a -> Now s (Now s a)
constMemo n = syncIO $ runMemo <$> newMVar (Left n) where
  runMemo m = do v <- syncIO $ takeMVar m
                 v' <- case v of
                         Left x  -> x 
                         Right x -> return x
                 syncIO $ putMVar m (Right v')
                 return v'

instance Functor (Behaviour s) where
  fmap = liftM

instance Applicative (Behaviour s) where
  pure = return
  (<*>) = ap




