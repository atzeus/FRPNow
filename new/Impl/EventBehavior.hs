{-# LANGUAGE LambdaCase #-}
module EventBehavior where

import Control.Applicative hiding (Const)
import Control.Monad

class (Applicative m, Monad m) => TimeEnv m where
  planM :: E m (m a) -> m (E m a)
  again :: (x -> m x) -> m x -> m x
  again _ x = x

data E m a = E (m (E m a))
           | Occ a
           | Never

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
  (E m)    >>= f = memoE ((>>= f) <$> m)

minTime :: TimeEnv m => E m a -> E m b -> E m ()
minTime Never r      = () <$ r
minTime l Never      = () <$ l
minTime (Occ _) _    = Occ ()
minTime _ (Occ _)    = Occ ()
minTime (E m) (E n)  = E $ minTime <$> m <*> n


memoE :: TimeEnv m => m (E m a) -> E m a
memoE m = E (again runEvent m)

-- Behaviors
-- strictness alert: do not runEvent the tail of a behavior we just got!

data B m a = B (m (a, E m (B m a)) )
           | Const a

runB :: TimeEnv m => B m a -> m (a, E m (B m a))
runB (Const x) = return (x, never)
runB (B m)     = m

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
   Never   -> runB b
   Occ   x -> runB x
   e'      -> do (h,t) <- runB b
                 return (h, switchEv t e')

switchEv :: TimeEnv m => E m (B m a) -> E m (B m a) -> E m (B m a)
switchEv l r = ((pure undefined `switch` l) `switch` r) <$
               (minTime l r)



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
