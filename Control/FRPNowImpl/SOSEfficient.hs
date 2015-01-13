{-# LANGUAGE FlexibleContexts, TypeFamilies, LambdaCase #-}
module SOSEfficient where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import System.IO.Unsafe


data Event m a = E { runEvent' :: m (Either (Event m a) a) }
               | Never

data EMemoOutcome m a = EMemoAgain (m (EMemoOutcome m a))
                      | ESameAs (Event m a)
                      | EDone a

memoE :: MonadIO m => m (EMemoOutcome m a) -> Event m a
memoE m = E $ runMemo where
  mem = unsafePerformIO $ newIORef m
  {-# NOINLINE mem #-}  
  runMemo = 
    do m <- liftIO $ readIORef mem 
       liftIO $ writeIORef mem (error "<<FRP loop>>")
       c <- m
       (res,m') <- case c of
         EDone a       -> return (Right a         , return c          )
         EMemoAgain m' -> return (Left $ E runMemo, m'                )
         ESameAs Never -> return (Left Never      , return c          )
         ESameAs (E e) -> e >>= \case
              Left e' -> return (Left e'         , return (ESameAs e'))
              Right a -> return (Right a         , return (EDone a)   )
       liftIO $ writeIORef mem m'
       return res
{-# NOINLINE memoE #-}  

never :: Event m a
never = Never

instance MonadIO m => Monad (Event m) where
  return x = E $ return $ Right x
  Never >>= f = Never
  (E m) >>= f = memoE $ bind m f where
    bind m f = m >>= return .  \case
        Left Never  -> ESameAs Never
        Left (E m') -> EMemoAgain (bind m' f)
        Right x     -> ESameAs (f x)
                    
{- This is symmetric! 
  
 ( note that first :: Event a -> Event a -> Event a
    is _not_ symmetric, have to decide in case  
    of simultaneity )
-} 
minTime :: MonadIO m => Event m a -> Event m b -> Event m ()
minTime Never r = () <$ r
minTime l Never = () <$ l
minTime (E l) (E r) = memoE (min l r) where
  min l r = r >>= \case
       Right _     -> return $ EDone ()
       Left Never  -> return $ ESameAs $ () <$ (E l)
       Left (E r') -> l >>= return . \case
         Right _     -> EDone ()
         Left Never  -> ESameAs $ () <$ (E r')
         Left (E l') -> EMemoAgain (min l' r')

data Behavior m a = B { runBehavior ::  m (a, Event m (Behavior m a)) }

data BMemoOutcome m a = BMemoAgain a (Event m (m (BMemoOutcome m a)))
                      | BSameAs (Behavior m a)


memoB :: MonadIO m => m (BMemoOutcome m a) -> Behavior m a
memoB m = B $ runMemo where
  mem = unsafePerformIO $ newIORef m
  {-# NOINLINE mem #-}  
  runMemo = 
    do m <- liftIO $ readIORef mem 
       liftIO $ writeIORef mem (error "<<FRP loop>>")
       c <- m
       (res,m') <- case c of
         BMemoAgain h t -> return ( (h, B runMemo <$ t), again h t)
         BSameAs b -> do (h,t) <- runBehavior b
                         return ((h,t), againSameAs b h t)
       liftIO $ writeIORef mem m'
       return res
  again h Never = return (BMemoAgain h Never)
  again h (E e) = e >>= \case
            Left e' -> return (BMemoAgain h e')
            Right x -> x
  againSameAs _ h Never = return (BMemoAgain h Never)
  againSameAs b h (E e) = e >>= \case
            Left _ -> return (BSameAs b)
            Right x -> return (BSameAs x)
{-# NOINLINE memoB #-}  

instance MonadIO m => Monad (Behavior m) where
  return x = B $ return (x, never)
  m >>= f  = memoB $ bind m f where
    bind m f = 
       do (h ,t ) <- runBehavior m
          (fh,ft) <- runBehavior (f h)
          let e   = switchEv ft ((`bind` f) <$> t)
          return $ BMemoAgain fh e

                    
                   
-- associative! 
-- This means, denotationally, 
-- switchEv (tl,bl) (tr,br) 
--  | tr <= tl = (tr, br)
--  | otherwise = (tl, bl `switch` (tr,br))                              
switchEv :: MonadIO m => Event m (Behavior m a) -> Event m (m (BMemoOutcome m a)) -> Event m (m (BMemoOutcome m a))
switchEv Never r = r
switchEv l Never = return . BSameAs <$> l
switchEv l@(E lm)  r@(E rm) = b <$ minTime l r where
 b = rm >>= \case
      Right x -> x
      Left r' -> lm >>= \case
          Right lb -> do (h,t) <- runBehavior lb
                         return $ BMemoAgain h (switchEv t r')
                 

switch :: MonadIO m => Behavior m a -> Event m (Behavior m a) -> Behavior m a
switch b Never = b
switch b (E e) = memoB $ e >>= \case
  Right x  -> return $ BSameAs x
  Left  e -> do (h,t) <- runBehavior b
                let e' = return . BSameAs <$> e
                return $ BMemoAgain h (switchEv t e')

class Monad m => Plan m where
  plan :: Event m (m a) -> m (Event m a)

whenJust :: (MonadIO m, Plan m) => Behavior m (Maybe a) -> Behavior m (Event m a)
whenJust b = memoB (wj b) where
  wj b = liftM (\(h,t) -> BMemoAgain h t) (whenJustm b) where
    whenJustm b = 
     do (h, t) <- runBehavior b
        case h of
         Just x -> return (return x, wj <$> t)
         Nothing -> do en <- plan (whenJustm <$> t)
                       let h = en >>= fst 
                       let t' = en >>= snd
                       return (h, t')





instance MonadIO m => Functor (Behavior m) where fmap = liftM
instance MonadIO m => Applicative (Behavior m) where pure = return ; (<*>) = ap

