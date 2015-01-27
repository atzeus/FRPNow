{-# LANGUAGE LambdaCase #-}
module SOSTT where

import Control.Applicative
import Control.Monad

class Monad m => Plan m where
  plan     :: Event m (m a) -> m (Event m a)

class Monad m => MemoTime m where
  memoTime :: (x -> m x) -> m x -> m x
  memoTime _ m = m

data Event m a = E (m (Either (Event m a) a))
               | Never

maybeToEv :: Monad m => m (Maybe a) -> Event m a
maybeToEv m = let x = E $ m >>= return . \case
                       Just x -> Right x
                       Nothing -> Left x
              in x

runEvent Never = return $ Left Never
runEvent (E m) = m

againE :: Monad m => Either (Event m a) a -> m (Either (Event m a) a)
againE (Right x) = return $ Right x
againE (Left m)  = runEvent m

memoE :: MemoTime m => m (Either (Event m a) a) -> m (Either (Event m a) a)
memoE = memoTime againE

never :: Event m a
never = Never

instance MemoTime m => Monad (Event m) where
  return x = E $ return $ Right x
  Never >>= f = Never
  (E m) >>= f = E $ memoE $ m >>= \case
      Left m  -> return (Left $ m >>= f)
      Right x -> runEvent (f x) 

minTime :: MemoTime m => Event m a -> Event m b -> Event m ()
minTime Never r = () <$ r
minTime l Never = () <$ l
minTime (E l) (E r) = E $ memoE $ 
 r >>= \case
   Right _ -> return (Right ())
   Left r  -> l >>= \case
     Right _ -> return (Right ())
     Left l  -> return (Left (minTime l r))

-- Behaviors

data Behavior m a = B { runBehavior ::  m (a, Event m (Behavior m a)) }

againB :: Monad m => (a, Event m (Behavior m a)) -> m (a, Event m (Behavior m a))
againB (h, t) = runEvent t >>= \case
                 Left m -> return (h,m)
                 Right x -> runBehavior x

memoB :: MemoTime m => m (a, Event m (Behavior m a)) -> m (a, Event m (Behavior m a))
memoB = memoTime againB

instance MemoTime m => Monad (Behavior m) where
  return x = B $ return (x, never)
  m >>= f  = B $ memoB $ 
     do (h,t) <- runBehavior m
        runBehavior $ f h `switch` ((>>= f) <$> t)

switch :: MemoTime m =>  Behavior m a -> Event m (Behavior m a) -> Behavior m a
switch b Never = b
switch b (E e) = B $ memoB $ e >>= \case
  Right x  -> runBehavior x
  Left e   -> do (h,t) <- runBehavior b
                 return (h, switch b e <$ minTime t e)

whenJust :: (Plan m, MemoTime m) => Behavior m (Maybe a) -> Behavior m (Event m a)
whenJust b = B $ memoB $ 
  do (h, t) <- runBehavior b
     case h of
      Just x -> return (return x, whenJust <$> t)
      Nothing -> do en <- plan (runBehavior . whenJust <$> t)
                    return (en >>= fst, en >>= snd)


instance MemoTime m => Functor (Event m) where fmap = liftM
instance MemoTime m => Applicative (Event m) where pure = return ; (<*>) = ap

instance MemoTime m => Functor (Behavior m) where fmap = liftM
instance MemoTime m => Applicative (Behavior m) where pure = return ; (<*>) = ap


