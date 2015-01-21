module PushPullCompat where

import qualified Control.FRPNow as Now


type Future   a = Now.Event a
type Reactive a = Now.Behavior a
type Behavior a = Now.Behavior (Time -> a)
type Event    a = Now.EventStream a

-- Differences

-- No monoid for future: could be done, but requires timestamps

{- No Monad for Event: inherently leaky 

  joinEv :: Event (Event a) -> Event a

  merges all events (streams) since the start(!) into a single one

  this requires us to remeber all events since the start,
  in case want to apply joinEv.

-}

{-

* Push-pull frp does not support whenJust
* push-pull frp supports inherently leaks combinators:

accumR :: a → Event (a → a) → Reactive a
accumE :: a → Event (a → a) → Event a


With us, this becomes:

fold    :: (a -> b -> a) -> a -> EventStream b -> Behavior (Behavior a)
scanlEv :: (a -> b -> a) -> a -> EventStream b -> Behavior (EventStream a)

-}




