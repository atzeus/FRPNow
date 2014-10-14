data Behaviour a = a `Switch` (Event (Behaviour a)) 

data TimeStamp = Always | Time Double | Never
data Event a = Ev TimeStamp a

instance Monad (Event a) where
  return = Ev Always 
  (Ev t1 a) >>= f = let Ev t2 x = f a
                               in Ev (max t1 t2) x

instance Monad (Behaviour a) where
  return x = x `Switch` Never
  (h `Switch` t) >>= f = f h `Switch` (fmap (>>= f) e)

instance MonadFix (Behaviour a) where
  mfix f = let (a `Switch` t) = f a 

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust (Nothing `Switch` t) = plan $ t >>= whenJust
whenJust (Just x `Switch` t)     = pure (return x) `Switch` fmap whenJust t

plan :: Event (Behaviour a) -> Behaviour (Event a)
plan (Ev t f) = toEv t (dropTill f) where
  dropTill x@(h `Switch` (Ev t2 s)) 
	| t2 <= t = loop s
         | otherwise = x 
   toEv (h `Switch` (Ev t2 s)) t = Ev t h `Switch` toEv s t2  


