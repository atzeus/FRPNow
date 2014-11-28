
import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Either
import Control.FRPNow hiding (Behaviour, switch,whenJust, seqS, beforeSwitch, delay , first)

infixr 3 :-> 

first :: Event a -> Event b -> Event (Either a b)
first = undefined


delay :: Event a -> Event a
delay = undefined


data Behaviour a = (:->) { headB :: a, tailB ::  Event (Behaviour a) }

instance Monad Behaviour where
  return x        = x :-> never
  (h :-> t) >>= f = f h `switch` fmap (>>= f) t

instance MonadFix Behaviour where
  mfix f = fix (head . beforeSwitch . f)

switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch (h :-> t) e = h :-> (either (`switch` e) id) <$> (first t e) 

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust (h :-> t) = 
  let t' = fmap whenJust t
  in case h of
   Just x  -> pure x :-> t'
   Nothing -> (t' >>= headB) :-> (t' >>= tailB)

seqS :: Behaviour x -> Behaviour a -> Behaviour a
seqS  s@(sh :-> st) b@(h :-> t) = 
  (sh `seq` h) :-> (either (`seqS` b) (s `seqS`)) <$> (first st t) 

beforeSwitch :: Behaviour a -> Behaviour a
beforeSwitch (h :-> t) = h :-> delay ($> t)

