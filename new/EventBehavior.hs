{-# LANGUAGE LambdaCase #-}
module EventBehavior where

import Control.Applicative hiding (Const)
import Control.Monad


data E m a = E (m (E m a))
           | Occ a
           | Never

runEvent :: Monad m => E m a -> m (E m a)
runEvent (Occ a) = return (Occ a)
runEvent Never   = return Never
runEvent (E m)   = m 

never :: E m a
never = Never

instance (Applicative m, Monad m) => Monad (E m) where
  return = Occ 
  Never    >>= f = Never
  (Occ x)  >>= f = f x
  (E m)    >>= f = memoE ((>>= f) <$> m)

minTime :: (Applicative m, Monad m) => E m a -> E m b -> E m ()
minTime Never r      = () <$ r
minTime l Never      = () <$ l
minTime (Occ _) _    = Occ ()
minTime _ (Occ _)    = Occ ()
minTime (E m) (E n)  = E $ minTime <$> m <*> n


-- Behaviors
-- strictness alert: do not runEvent the tail of a behavior we just got!

data B m a = B (m (a, E m (B m a)) )
           | Const a

runB :: Monad m => B m a -> m (a, E m (B m a))
runB (Const x) = return (x, never)
runB (B m)     = m

instance (Applicative m, Monad m) => Monad (B m) where
  return = Const
  (Const x) >>= f = f x 
  (B m)     >>= f = memoB $
    do (h,t) <- m 
       (fh,th) <- runB (f h)
       return (fh, switchEv th ((>>= f) <$> t))
            
switch :: (Applicative m, Monad m) =>  B m a -> E m (B m a) -> B m a
switch b Never   = b
switch _ (Occ b) = b
switch b (E e)   = memoB $ e >>= \case
   Never   -> runB b
   Occ   x -> runB x
   e'      -> do (h,t) <- runB b
                 return (h, switchEv t e')

switchEv :: (Applicative m, Monad m) => E m (B m a) -> E m (B m a) -> E m (B m a)
switchEv l r = ((pure undefined `switch` l) `switch` r) <$
               (minTime l r)

class (Applicative m, Monad m) => Plan m where
  plan :: E m (m a) -> m (E m a)

whenJust :: Plan m => B m (Maybe a) -> B m (E m a)
whenJust (Const Nothing)  = pure never
whenJust (Const (Just x)) = pure (pure x)
whenJust (B b) = memoB $ 
  do (h, t) <- b
     case h of
      Just x -> return (return x, whenJust <$> t)
      Nothing -> do en <- plan (runB . whenJust <$> t)
                    return (en >>= fst, en >>= snd)

memoB = B
memoE = E

instance (Applicative m, Monad m) => Functor (E m) where
  fmap = liftM

instance (Applicative m, Monad m) =>  Applicative (E m) where
  pure = return
  (<*>) = ap

instance (Applicative m, Monad m) =>  Functor (B m) where
  fmap = liftM

instance (Applicative m, Monad m) =>  Applicative (B m) where
  pure = return
  (<*>) = ap
