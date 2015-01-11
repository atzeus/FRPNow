{-# LANGUAGE LambdaCase #-}
module Control.FRPNowImpl.SOS where

import Control.Applicative
import Control.Monad

data Event m a = E { runEvent :: m (Either (Event m a) a) }

never :: Monad m => Event m a
never = E (return (Left never))

instance Monad m => Monad (Event m) where
  return x = E $ return $ Right x
  m >>= f = E $ runEvent m >>= \case
      Left  m' -> return $ Left $ m' >>= f
      Right x  -> runEvent (f x)

{- This is symmetric! 
  
 ( note that first :: Event a -> Event a -> Event a
    is _not_ symmetric, have to decide in case  
    of simultaneity )
-} 
minTime :: Monad m => Event m a -> Event m b -> Event m ()
minTime (E lm) (E rm) = E $ rm >>= \case
  Right _ -> return $ Right ()
  Left r' -> lm >>= \case  
       Right _ -> return $ Right ()
       Left l' -> return $ Left $ minTime l' r'

data Behavior m a = B { runBehavior :: m (a, Event m (Behavior m a)) }

instance Monad m => Monad (Behavior m) where
  return x = B $ return (x, never)
  m >>= f  = B $ do (h,t) <- runBehavior m
                    runBehavior $ f h `switch` ((>>= f) <$> t)
                   
-- associative! 
-- This means, denotationally, 
-- switchEv (tl,bl) (tr,br) 
--  | tr <= tl = (tr, br)
--  | otherwise = (tl, bl `switch` (tr,br))                              
switchEv :: Monad m => Event m (Behavior m a) -> Event m (Behavior m a) -> Event m (Behavior m a)
switchEv l r = b <$ minTime l r where
 b = B $ runEvent r >>= \case
   Right rb -> runBehavior rb
   Left r' -> runEvent l >>= \case
     Right lb -> do (hl,tl) <- runBehavior lb
                    return (hl, switchEv tl r')
     Left l' -> error "Cannot happen"
     
switch :: Monad m=>  Behavior m a -> Event m (Behavior m a) -> Behavior m a
switch b e = 
 B $ runEvent e >>= \case
  Right x -> runBehavior x
  Left e -> do (h,t) <- runBehavior b
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

