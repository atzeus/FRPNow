{-# LANGUAGE LambdaCase #-}
module EventBehavior where

--import Control.Applicative
import Control.Monad

data E m a = E (m (E m a))
           | Occ a
           | Never

never :: E m a
never = Never

(<$>) = liftM
(<$) = liftM . const

instance Monad m => Monad (E m) where
  return = Always 
  Never      >>= f = Never
  (Always x) >>= f = f x
  (E m)      >>= f = memoE ((>>= f) <$> m)

minTime :: Event a -> Event b -> Event ()
minTime Never r      = () <$ r
minTime l Never      = () <$ l
minTime (Always _) _ = Always ()
minTime _ (Always _) = Always ()
minTime (E m) (E n)  = E $ minTime <$> m <*> n


-- Behaviors
-- strictness alert: do not runEvent the tail of a behavior we just got!

data B m a = B (m (a, (E m (B m a)))
           | Const x

runB :: B m a -> m (a, E m (B m a))
runB (Const x) = return (x, never)
runB (B m)     = m

instance Monad m => Monad (B m) where
  return = Const
  (Const x) >>= f = f x 
  (B m)     >>= f = memoB $
    do (h,t) <- m 
       (fh,th) <- runB (f h)
       return (fh, switchEv th ((>>= f) <$> t))
            
switch :: Monad m =>  B m a -> E m (B m a) -> B m a
switch b Never       = b
switch _ (Always b)  = b
switch b (E e)   = memoB $ e >>= \case
   Never    -> runB b
   Always x -> runB x
   e'       -> do (h,t) <- runB b
                  return (h, switchEv t e')

switchEv :: Monad m => E m (B m a) -> E m (B m a) -> E m (B m a)
switchEv l r = (pure undefined `switch` l `switch` r) <$> 
               (minTime l r)


whenJust :: Plan m => B m (Maybe a) -> B m (E m a)
whenJust (Const Nothing)  = pure never
whenJust (Const (Just x)) = pure (pure x)
whenJust (B b) = memoB $ 
  do (h, t) <- b
     case h of
      Just x -> return (return x, whenJust <$> t)
      Nothing -> do en <- plan (runBehavior . whenJust <$> t)
                    return (en >>= fst, en >>= snd)

