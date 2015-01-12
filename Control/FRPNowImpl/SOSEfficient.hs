{-# LANGUAGE LambdaCase #-}
module SOSEfficient where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import System.IO.Unsafe


data Event m a = E { runEvent' :: m (EventState m a) }
               | Never

data EventState m a = Done a
                    | SameAs (Event m a)
                    | NotYet (Event m a)

runEvent :: Monad m => Event m a -> m (EventState m a)
runEvent Never = return $ NotYet Never
runEvent (E m) = m >>= \case
     Done a -> return $ Done a
     SameAs x -> runEvent x
     NotYet e -> return $ NotYet e

never :: Monad m => Event m a
never = Never

instance MonadIO m => Monad (Event m) where
  return x = E $ return $ Done x
  Never >>= f = Never
  m >>= f = E $ runEvent m >>= return . \case
        NotYet m' -> NotYet (m' >>= f)
        Done x -> SameAs (f x)
                    
 
{- This is symmetric! 
  
 ( note that first :: Event a -> Event a -> Event a
    is _not_ symmetric, have to decide in case  
    of simultaneity )
-} 
minTime :: MonadIO m => Event m a -> Event m b -> Event m ()
minTime Never r = () <$ r
minTime l Never = () <$ l
minTime l r = E $ runEvent r >>= \case
      Done _     -> return $ Done ()
      NotYet r'  -> 
       runEvent l >>= \case
         Done _ -> return $ Done ()
         NotYet l' -> return $ NotYet (minTime l' r')

data Behavior m a = B { runBehavior' ::  m (BehaviorState m a) }

data BehaviorState m a = SameAsB (Behavior m a)
                       | HeadTail a (Event m (Behavior m a))

runBehavior :: Monad m => Behavior m a -> m (a, Event m (Behavior m a)) 
runBehavior (B m) = m >>= \case
   SameAsB b -> runBehavior b
   HeadTail h t -> return (h,t)

instance MonadIO m => Monad (Behavior m) where
  return x = B $ return (HeadTail x  never)
  m >>= f  = B $ do (h,t) <- runBehavior m
                    let x = f h
                    case t of
                      Never -> return (SameAsB x)
                      t'     -> runBehavior' $ f h `switch` ((>>= f) <$> t')
                    
                   
-- associative! 
-- This means, denotationally, 
-- switchEv (tl,bl) (tr,br) 
--  | tr <= tl = (tr, br)
--  | otherwise = (tl, bl `switch` (tr,br))                              
switchEv :: MonadIO m => Event m (Behavior m a) -> Event m (Behavior m a) -> Event m (Behavior m a)
switchEv Never r = r
switchEv l Never = l
switchEv l r = b <$ minTime l r where
 b = B $ runEvent r >>= \case
   Done rb -> return $ SameAsB rb
   NotYet r' -> runEvent l >>= \case
     Done lb -> do (hl,tl) <- runBehavior lb
                   return $ HeadTail hl (switchEv tl r')
     _  -> error "Cannot happen"
     
switch :: MonadIO m => Behavior m a -> Event m (Behavior m a) -> Behavior m a
switch b Never = b
switch b e = 
 B $ runEvent e >>= \case
  Done x -> return $ SameAsB x
  NotYet e -> do (h,t) <- runBehavior b
                 return $ HeadTail h (switchEv t e)

class Monad m => Plan m where
  plan :: Event m (m a) -> m (Event m a)

whenJust :: (MonadIO m, Plan m) => Behavior m (Maybe a) -> Behavior m (Event m a)
whenJust b = B $ liftM (\(h,t) -> HeadTail h t) (whenJustm b) where
  whenJustm b = 
    do (h, t) <- runBehavior b
       case h of
        Just x -> return (return x, whenJust <$> t)
        Nothing -> do en <- plan (whenJustm <$> t)
                      let h = en >>= fst 
                      let t' = en >>= snd
                      return (h, t')

againE :: Monad m => EventState m a -> Event m a
againE (SameAs Never) = Never
againE (NotYet Never) = Never
againE x@(Done a)     = E $ return x
againE (NotYet e)     = E $ runEvent' e 
againE (SameAs e)     = E $ runEvent' e >>= \case
                SameAs e' -> runEvent'(againE (SameAs e'))
                _         -> return $ SameAs e


againB :: Monad m =>  BehaviorState m a -> Behavior m a
againB (HeadTail h t) = B $ runEvent t >>= \case
                               Done x -> runBehavior' x
                               NotYet t' -> return $ HeadTail h t'
againB (SameAsB b) = B $ runBehavior' b >>= \case
                HeadTail h t -> return $ SameAsB b
                SameAsB b' -> runBehavior' (againB (SameAsB b'))

memoB :: MonadIO m => Behavior m a -> Behavior m a
memoB b = B $ runMemo where
  mem = unsafePerformIO $ newIORef b
  {-# NOINLINE mem #-}  
  runMemo = 
    do es <- liftIO $ readIORef mem 
       res <- runBehavior' es
       liftIO $ writeIORef mem (againB res)
       return res
{-# NOINLINE memoB #-}  

memoE :: MonadIO m => m (EventState m a) -> Event m a
memoE e = E $ runMemo where
  mem = unsafePerformIO $ newIORef e
  {-# NOINLINE mem #-}  
  runMemo = 
    do es <- liftIO $ readIORef mem 
       res <- es
       liftIO $ writeIORef mem (againE res)
       return res
{-# NOINLINE memoE #-}  

instance MonadIO m => Functor (Event m) where fmap = liftM
instance MonadIO m => Applicative (Event m) where pure = return ; (<*>) = ap

instance MonadIO m => Functor (Behavior m) where fmap = liftM
instance MonadIO m => Applicative (Behavior m) where pure = return ; (<*>) = ap

