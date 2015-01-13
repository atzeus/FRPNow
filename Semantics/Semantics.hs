{-# LANGUAGE  TypeSynonymInstances, TypeOperators #-}

module Semantics where

import Control.Applicative
import Control.Monad.Fix
import Data.Maybe

type Time = Double
inf = 1/0

type Behaviour a = Time -> a
data Event a = a :@ Time



instance Monad Event where -- writer monad
  return a = a :@ (-inf)
  (a :@ t) >>= f = b :@ max t t' where (b :@ t') = f a

switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch b (s :@ ts) t = if t >= ts then s t else b t


whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust f t = let t' = undefined -- min { t' >= t | isJust (f t') }
               in {-if exists z (t <= z <= t') s.t. f z == bottom
                  then bottom
                  else -} fromJust (f t') :@ t'

seqB :: Behaviour x -> Behaviour a -> Behaviour a
seqB s b t = s t `seq` b t

dfix :: (Behaviour a -> Behaviour a) -> Behaviour a
dfix f = loop 0 (f undefined)
  loop i b = loop (i + 1) $ f (limit i b)
  limit i b = \t -> snd (b t) i

eqFix f a = let x = f a
            in if x = a
               then x
               else eqFix f a

 let e = when (== 1) b
     b = e >>= \e2 ->  pure 0 `switch` (1 <$ e1) `switch` (2 <$ e2)
 in b



-- i.e. fix until (delay b) == b
-- with property : advance . delay = id
-- but delay . advance = delay
bfixm :: MonadFix m => (Behaviour a -> m (Behaviour a)) -> m (Behaviour a)
bfixm f =  



{- reader monad
   this monad & monadfix is listed in the
   standard libraries as Monad ((->) r) 
   the only difference is that r = time
instance Monad Behaviour where
    return = const
    f >>= k = \ r -> k (f r) r -- difference! Strict in f! (not in (f r))


instance MonadFix Behaviour where
   mfix f = \t -> let a = f a t in a
-}

--- Derived combinators

step :: a -> Event (Behaviour a) -> Behaviour a
step a s = pure a `switch` s

toBehaviour :: Event (Behaviour a) -> Behaviour (Maybe a)
toBehaviour e = Nothing `step` fmap (fmap Just) e

plan :: Event (Behaviour a) -> Behaviour (Event a)
plan = whenJust . toBehaviour

-- IO Stuff

-- newtype Now a = Now { runNow :: Time -> IO a }
--              ReaderT Time (IO a)
-- the runNow function conceptually runs 
-- the given IO action at the given time
-- in practice we must guaratee this in the future

type Now a = Time -> IO a





evNow :: Event a -> Now (Maybe a)
evNow (a :@ t) = \n -> return $ if n >= t then Just a else Nothing

-- time at which the event occurs is guaranteed
-- to be > now, i.e. any consequetive
-- invocation of evNow on the resulting 
-- event in _this_ Now monad
-- will give nothing
asyncIO :: IO a -> Now (Event a)
asyncIO m = \t -> do undefined -- ....
                     -- return (a :@ te) -- te > t


first :: Event a -> Event a -> Now (Event a)
first (a :@ ta) (b :@ tb) t 
  | t >= min ta tb   = return $ b :@ t
  | tb <= ta         = return $ b :@ tb
  | otherwise        = return $ a :@ ta


planIO :: Event (Now a)-> Now (Event a)
planIO (m :@ t) = \n -> 
  let t' = max t n
  in do v <- m t'; return (v :@ t')

curIO :: Behaviour a -> Now a
curIO b = \t -> return (b t)





---- Monad - applicative - functor  stuff

instance Functor Event where
  fmap f (a :@ t) = f a :@ t

instance Applicative Event where
  pure = return
  f <*> g = do x <- f ; y <- g ; return (x y)
