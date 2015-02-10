{-# LANGUAGE ScopedTypeVariables, ExistentialQuantification, Rank2Types, GeneralizedNewtypeDeriving #-}
import Debug.Trace

import Control.Applicative

data P s a = P a  

instance Functor (P s) where
  fmap f (P i) = P (f i)

instance Applicative (P s) where
  pure = P
  (P f) <*> (P x) = P (f x)

bla :: P s Int
bla = let x = P (trace "hallo" $ 1)
      in (+) <$> x <*> x

getPhant :: (forall s. P s a) -> a
getPhant (P i) = i

-- Hallo only printed once: too much sharing!
main1 = do putStrLn (show $ getPhant bla)
           putStrLn (show $ getPhant bla)   

bla2 :: Applicative f => f Int
bla2 = let x = pure (trace "hallo" $ 1)
       in (+) <$> x <*> x

data Ident a = I a 

instance Functor Ident where
  fmap f (I i) = I (f i)

instance Applicative Ident where
  pure = I
  (I f) <*> (I x) = I (f x)

extract :: forall a. (forall f. Applicative f => f a) -> a
extract p = val where
        p' :: Ident a
        p' = p
        I val = p'
-- Hello printed twice: just enough sharing!
main = do putStrLn (show $ extract bla2)
          putStrLn (show $ extract bla2)   


class (MonadFix b, Monad e) => FRP b e where
  never    :: e a
  switch   :: b a -> e (b a) -> b a
  whenJust :: b (Maybe a) -> b (e a)

class FRP b e => RealWorldFRP b e n where
  async    :: IO a -> n (e a)
  schedule :: e (n a) -> n (e a)

runFRP :: (forall b e n. RealWorldFRP b e n => n (e a)) -> IO a

type Time = Double

class FRP b e => VirtualFRP b e where
  makeEvent :: Time -> a -> e a

-- Historical events and behaviors, cannot implement FRP 
-- because they do not reveal lowerbounds, will diverge
-- on some uses of whenJust
data HEvent    a = Occ Time a | Never
data HBehavior a = a `Step` (HEvent (HBehavior a))

runVirtualFRP :: Eq a => (forall b e. VirtualFRP b e => b a) -> HBehavior a


