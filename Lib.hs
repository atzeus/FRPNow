{-# LANGUAGE GADTs, TupleSections #-}

module Lib where

import IO.Now
import Race
import Control.Applicative
import TermM

cur :: Event s a -> Now s (Maybe a)
cur e = now $ toBehaviour e

toBehaviour :: Event s a -> Behaviour s (Maybe a)
toBehaviour e = pure Nothing `switch` fmap (pure . Just) e

race :: Event s a -> Event s b -> Now s (Event s (Race a b))
race a b = whenJust $ combineMaybe <$> toBehaviour a <*> toBehaviour b

data TermBehaviourPrim s a b where
  Until     :: Behaviour s a -> Event s b -> TermBehaviourPrim s a b
  LiftNow   :: Now s x                -> TermBehaviourPrim s a x


type TermBehaviourExp s a = TermM (TermBehaviourPrim s a)
type TermBehaviour s a b = (Behaviour s a, Event s b)

runTermBehaviour :: TermBehaviourExp s a b -> Now s (TermBehaviour s a b)
runTermBehaviour e = case viewTermM e of
  Return x -> return (pure undefined, return x)
  m :>>= f -> do (b,e) <- handlePrim m
                 let again x = runTermBehaviour (f x)
                 e2 <- plan $ fmap again e
                 let e2e = e2 >>= snd
                 let e2b = fmap fst e2
                 return (b `switch` e2b, e2e)
 where handlePrim (Until b e) = return (b, e)
       handlePrim (LiftNow n) = do x <- n; return  (pure undefined, return x) 
  
