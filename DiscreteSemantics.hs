import Control.Applicative
import Control.Monad.Fix


data Behaviour a = Step { getHead :: a, getTail :: Event (Behaviour a) }

instance Monad Behaviour where
  return x = x `Step` never
  (h `Step` t) >>= f = f h `switch` fmap (>>= f) t

-- this might be correct?
instance MonadFix Behaviour where
  mfix f = let b = loop b in b where
    loop (a `Step` t) = f a `switch` fmap loop t

(h `Step` t) `switch` e = h `Step` (fmap nxt $ race t e) where
  nxt (Left b)  = b `switch` e
  nxt (Right b) = b

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust (Just x  `Step` e) = pure x `Step` fmap whenJust e
whenJust (Nothing `Step` e) = let e' = fmap whenJust e
                              in  (e' >>= getHead) `Step` (e' >>= getTail)

plan :: Event (Behaviour a) -> Behaviour (Maybe a)
plan e = return Nothing `switch` fmap (fmap Just) e


-- external interface..,
sample :: Event () -> Behaviour a -> Event a
sample e = loop where
  loop b@(h `Step` t) = 
     do v <- race t e
        case v of
           Left b'  -> loop b'
           Right () -> return h




























data Event a

instance Functor Event
instance Applicative Event
instance Monad Event

race :: Event a -> Event b -> Event (Either a b)
race = undefined
never :: Event a
never = undefined




instance Functor Behaviour where 
  fmap f (a `Step` t) = f a `Step` fmap (fmap f) t

