
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Either
import Control.FRPNow hiding (Behaviour, switch,whenJust, seqS, beforeSwitch, delay , first)

infixr 3 :-> 


data Behaviour a = (:->) { headB :: a, tailB ::  Event (Behaviour a) }

instance Monad Behaviour where
  return x        = x :-> never
  (h :-> t) >>= f = f h `switch` fmap (>>= f) t

{-
instance MonadFix Behaviour where
  mfix f = fix (f . headB)
-}

switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch (h :-> t) e = h :-> (either (`switch` e) id) <$> (first t e) 

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust (h :-> t) = 
  let t' = fmap whenJust t
  in case h of
   Just x  -> pure x :-> t'
   Nothing -> (t' >>= headB) :-> t'

seqS :: Behaviour x -> Behaviour a -> Behaviour a
seqS  s@(sh :-> st) b@(h :-> t) = 
  (sh `seq` h) :-> ((s `seqS`) <$> t) 

