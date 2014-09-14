module Pure.Behaviour where

import Race
import Pure.Event
import Control.Applicative

-- has space/time leak!

data Behaviour a = a :| Event (Behaviour a)

instance Functor Behaviour where
  fmap f (h :| t) = f h :| fmap (fmap f) t

instance Applicative Behaviour where
  pure x = x :| never
  f@(fh :| ft) <*> x@(xh :| xt) = fh xh :| fmap tail (race ft xt)
    where   tail (Tie ftv xtv) = ftv <*> xtv
            tail (L ftv)       = ftv <*> x
            tail (R xtv)       = f <*> xtv

switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch (h :| t) e = h :| fmap nxt (race t e)
    where nxt  (Tie a b) = b
          nxt  (R b)     = b
          nxt  (L a)     = switch a e
