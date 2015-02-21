{-# LANGUAGE LambdaCase #-}
module Impl.EventBehavior where

import Control.Applicative hiding (Const)
import Control.Monad
import Debug.Trace

class (Applicative m, Monad m) => TimeEnv m where
  planM :: E m (m a) -> m (E m a)
  again :: (x -> m x) -> m x -> m x
  again _ x = x

data E m a = E (m (E m a))
           | Occ a
           | Never

curE :: TimeEnv m => E m a -> m (Maybe a)
curE e = runEvent e >>= return . \case
  Occ x -> Just x
  _     -> Nothing


fromMaybeM :: TimeEnv m => m (Maybe a) -> E m a
fromMaybeM m = E $ m >>= return . \case
   Just x -> Occ x
   _      -> fromMaybeM m
   

runEvent :: TimeEnv m => E m a -> m (E m a)
runEvent (Occ a) = return (Occ a)
runEvent Never   = return Never
runEvent (E m)   = m 

never :: E m a
never = Never

instance TimeEnv m => Monad (E m) where
  return = Occ 
  Never    >>= f = Never
  (Occ x)  >>= f = f x
  (E m)    >>= f = memoE $
    m >>= \case
      Never ->  return Never
      Occ x ->  runEvent (f x)
      e     ->  return (e >>= f)


memoE :: TimeEnv m => m (E m a) -> E m a
memoE m = E (again runEvent m)

-- Behaviors
-- strictness alert: do not runEvent the tail of a behavior we just got!

data B m a = B (m (a, E m (B m a)) )
           | Const a

runB :: TimeEnv m => B m a -> m (a, E m (B m a))
runB (Const x) = return (x, never)
runB (B m)     = m

curB :: TimeEnv m => B m a -> m a
curB b = fst <$> runB b

instance TimeEnv m => Monad (B m) where
  return = Const
  (Const x) >>= f = f x 
  (B m)     >>= f = memoB $
    do (h,t) <- m 
       (fh,th) <- runB (f h)
       return (fh, switchEv th ((>>= f) <$> t))
            
switch :: TimeEnv m =>  B m a -> E m (B m a) -> B m a
switch b Never   = b
switch _ (Occ b) = b
switch b (E e)   = memoB $ e >>= \case
   Occ   x -> runB x
   Never   -> runB b
   e'      -> do (h,t) <- runB b
                 return (h, switchEv t e')

switchEv :: TimeEnv m => E m (B m a) -> E m (B m a) -> E m (B m a)
switchEv l Never     = l
switchEv l (Occ r)   = Occ r
switchEv Never r     = r
switchEv (Occ x) r   = Occ (x `switch` r)
switchEv (E l) (E r) = memoE $ 
  r >>= \case
    Occ y -> return $ Occ y
    r' -> l >>= return . \case 
           Occ x -> Occ (x `switch` r')
           l'    -> switchEv l' r'



whenJust :: TimeEnv m => B m (Maybe a) -> B m (E m a)
whenJust (Const Nothing)  = pure never
whenJust (Const (Just x)) = pure (pure x)
whenJust (B b) = memoB $ 
  do (h, t) <- b
     case h of
      Just x -> return (return x, whenJust <$> t)
      Nothing -> do en <- planM (runB . whenJust <$> t)
                    return (en >>= fst, en >>= snd)

againB :: TimeEnv m => (a, E m (B m a)) -> m (a, E m (B m a))
againB (h,t) = runEvent t >>= \case
      Occ x -> runB x
      _     -> return (h,t)

memoB :: TimeEnv m => m (a, E m (B m a)) -> B m a
memoB m = B (again againB m)

instance TimeEnv m => Functor (E m) where
  fmap = liftM

instance TimeEnv m =>  Applicative (E m) where
  pure = return
  (<*>) = ap

instance TimeEnv m =>  Functor (B m) where
  fmap = liftM

instance TimeEnv m =>  Applicative (B m) where
  pure = return
  (<*>) = ap
