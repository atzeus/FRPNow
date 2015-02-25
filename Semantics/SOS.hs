{-# LANGUAGE LambdaCase #-}
module Control.FRPNowImpl.SOS where

import Control.Applicative
import Control.Monad

data Event m a = E { runEvent :: m (Maybe a) }

never :: Monad m => Event m a
never = E (return Nothing)

instance Monad m => Monad (Event m) where
  return x = E $ return (Just x)
  m >>= f = E $ runEvent m >>= \case
      Nothing  -> return Nothing
      Just x  -> runEvent (f x)

minTime :: Event m a -> Event m b -> Event m ()
minTime l r = E $ 
     runEvent r >>= \case
        Right _ -> return (Right $ Just ())
        Left r  -> runEvent l >>= \case
          Right _ -> return (Right $ Just ())
          Left l  -> return (

data Behavior m a = B { runBehavior :: m (a, Event m (Behavior m a)) }

instance Monad m => Monad (Behavior m) where
  return x = B $ return (x, never)
  m >>= f  = B $ do (h,t) <- runBehavior m
                    runBehavior $ f h `switch` ((>>= f) <$> t)
                   
     
switch :: Monad m=>  Behavior m a -> Event m (Behavior m a) -> Behavior m a
switch b e = 
 B $ runEvent e >>= \case
  Just x  -> runBehavior x
  Nothing -> do (h,t) <- runBehavior b
                return (h, switchEv t e)

class Monad m => Plan m where
  plan :: Event m (m a) -> m (Event m a)

whenJust :: Plan m => Behavior m (Maybe a) -> Behavior m (Event m a)
whenJust = B . whenJustm where
  whenJustm b = 
    do (h, t) <- runBehavior b
       case h of
        Just x -> return (return x, whenJust <$> t)
        Nothing -> do en <- plan (whenJustm <$> t)
                      let h = en >>= fst
                      let t' = en >>= snd
                      return (h,t')

instance Monad m => Functor (Event m) where fmap = liftM
instance Monad m => Applicative (Event m) where pure = return ; (<*>) = ap

instance Monad m => Functor (Behavior m) where fmap = liftM
instance Monad m => Applicative (Behavior m) where pure = return ; (<*>) = ap

