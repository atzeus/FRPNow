{-# LANGUAGE RecursiveDo, ScopedTypeVariables, LambdaCase #-}
module Control.FRPNowImpl.Behaviour(Behaviour,curIO, switch, whenJust, seqb, seqAlways) where
import Control.Applicative hiding (empty,Const)
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.FRPNowImpl.Event
import Control.FRPNowImpl.Now
import Data.Sequence
import Debug.Trace
import Data.Foldable (toList)
import Data.Maybe

infixr 3 :-> 
data BState s a = (:->) { headB :: Now s a , tailB :: Event s (Behaviour s a) }
                | SameAs (Behaviour s a)
                | Const a

again :: BState s a -> Behaviour s a
again x = B $ 
  case x of
   h :-> t -> evNow t >>= \case 
              Just b -> getHT b
              Nothing -> return x
   SameAs _ -> return x
   Const _  -> return x

getHTFull :: Behaviour s a -> Now s (BState s a)
getHTFull b = 
  getHT b >>= \case
          Const x -> return (pure x :-> never)
          SameAs b' -> getHTFull b'
          h :-> t  -> return (h :-> t)

curIO :: Behaviour s a -> Now s a
curIO b = do h :-> t <- getHTFull b
             h
   

newtype Behaviour s a = B { getHT :: Now s (BState s a) }


instance Monad (Behaviour s) where
  return a = B $ return (Const a)
  m >>= f = memo $ bind m f

bind m f = B $ getHT m >>= \case
       SameAs m' -> getHT (bind m' f)
       h :-> t -> do x <- h
                     let t' = (`bind` f) <$> t
                     getHT (f x `switch'` t')
       Const x -> return (SameAs (f x))

switch ::  Behaviour s a -> Event s (Behaviour s a) -> Behaviour s a
switch b e =  memo $  switch' b e

switch' b e = B  $ 
  evNow e >>= \case 
    Just a  -> return (SameAs a)
    Nothing -> getHT b >>= \case
        SameAs b' -> getHT (switch' b' e)
        h :-> t -> do let t' = (`switch'` e) <$> t
                      ts <- t' `firstObs` e
                      return $ h :-> ts
        Const x -> return (pure x :-> e)


whenJust' :: Behaviour s (Maybe a) -> Behaviour s (Event s a)
whenJust' b = B $ 
  getHT b >>= \case
      SameAs b' -> getHT (whenJust' b')
      Const x   -> return $ Const (maybe never return x)
      h :-> t   -> do let tw = whenJust' <$> t
                      return (getJust b :-> tw)

getJust b = getHT b >>= \case
     Const x -> return $ maybe never return x
     SameAs b' -> getJust b'
     h :-> t -> 
       do v <- h
          case v of
           Just a -> return (pure a)
           Nothing  -> join <$> planIOWeak (getJust <$> t)

seqb :: Behaviour s x -> Behaviour s a -> Behaviour s a
seqb l r = memo $  seqb' l r

seqb' l r = B $ getHT l >>= \case
  Const x   -> return $ SameAs r
  SameAs l' -> getHT (seqb' l' r)
  hl :-> tl   ->
       getHT r >>= \case
          Const y   -> return $ (hl >> return y) :-> ((`seqb` r) <$> tl)
          SameAs r' -> getHT (seqb' l r')
          hr :-> tr -> return $ (hl >> hr) :-> ((l `seqb`) <$> tr)

      
seqAlways :: Behaviour s a -> Now s ()
seqAlways b = 
  getHT b >>= \case
   Const x -> return ()
   SameAs b' -> seqAlways b'
   h :-> t -> planIOWeakKey b (seqAlways <$> t) >> return ()



memo :: Behaviour s a -> Behaviour s a
memo b = B $ runMemo  where
  mvar = unsafePerformIO $ newMVar b
  {-# NOINLINE mvar #-}  
  runMemo = 
    do b <- syncIO $ takeMVar mvar 
       v <- getHT b
       syncIO $ putMVar mvar (again v)
       return v
{-# NOINLINE memo #-}  

whenJust b = whenJust' b

instance Functor (Behaviour s) where
  fmap = liftM

instance Applicative (Behaviour s) where
  pure = return
  (<*>) = ap
