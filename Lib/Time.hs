{-# LANGUAGE TypeOperators #-}

module Lib.Time where

import Impl.GTKFRPNow
import Lib.Lib
import Lib.EventStream
import Swap
import Data.Sequence.BSeq
import Control.Applicative hiding (empty)
import Data.Time.Clock.POSIX
import Data.Foldable
import Debug.Trace

-- seconds
type Time = Double
type Duration = Double

type Sample a = (Time,a)
type History a = (a, BSeq (Time,a))

type TimeSlice a = BSeq (Time,a)

data Record a = Record { firstSample :: Sample a, rest :: BSeq (Sample a) } 

recordToList :: Record a -> [a]
recordToList (Record (_,x) t) = x : map snd (toList t)

getElapsedTimeSeconds :: IO Time
getElapsedTimeSeconds =  fromRational . toRational <$> getPOSIXTime

addSample :: History a -> Sample a -> History a
addSample (i,ss) s = (i, ss |> s)


fromTime :: Time -> History a  -> History a
fromTime t (a,s) =
 case viewl s of
   EmptyL -> (a, empty)
   (ts,x) :< f -> if ts <= t
                  then fromTime t (x,f)
                  else (a,s)

dropWhileSeq :: Sequence s => (a -> Bool) -> s a -> s a
dropWhileSeq f s = case viewl s of
                     EmptyL -> empty
                     h :< t | f h -> dropWhileSeq f t
                            | otherwise -> s

recordB :: Eq a => Clock -> Duration -> Behavior a -> Behavior (Behavior (BSeq (Time,a)))
recordB c d b = foldB addDrop empty ((,) <$> c <*> b)
  where  addDrop l (now,v) = let from = now - d
                             in (dropWhileSeq (\(t,_) -> t < from) l) |> (now,v)

trimTillTime :: Record a -> Time -> Record a
trimTillTime x@(Record (ft,fv) r) time = case viewl r of
           EmptyL                        -> x
           (st,sv) :< stail | ft < time || ft == st -> trimTillTime (Record (st,sv) stail) time
                            | otherwise  -> x
slice :: Clock -> a -> Stream a ->  Duration -> Behavior (Stream (Record a))
slice clock i s d = scanlEv addDrop (Record (0,i) empty) $ ((,) <$> clock) `fmapB` s
  where addDrop (Record i r) s@(t,_) = trimTillTime (Record i (r |> s)) (t -d)
{-
waitSeconds :: Duration -> IO ()
waitSeconds d = traceIO "gonna wait" >> threadDelay (round (d * 1000000)) >> traceIO "Bla"

getClock :: Duration -> Now (Behavior Time)
getClock minDelta = loop where
 loop =
   do now <- syncIO $ getElapsedTimeSeconds

      e <- async (waitSeconds minDelta)
      e' <- plan (loop <$ e)
      return (pure now `switch` e')
-}

localTime :: Behavior Time -> Behavior (Behavior Time)
localTime t = do n <- t
                 return ((\x -> x - n) <$> t)

timeFrac :: Behavior Time -> Duration -> Behavior (Behavior Double)
timeFrac t d = do t' <- localTime t
                  e <- when $ (>= d) <$> t'
                  let frac = (\x -> min 1.0 (x / d)) <$> t'
                  return (frac `switch` (pure 1.0 <$ e))


deltaTime :: Behavior Time -> Behavior (Event Time)
deltaTime time = do now <- time
                    ((\x -> x - now) <$>) <$> changeVal time


integral :: Behavior Time -> Behavior Double -> Behavior (Behavior Double)
integral time b =
  do cur <- b
     dt <- deltaTime time
     e <- plan (loop cur 0 <$> dt)
     return (pure 0 `switch` e) where
   loop prev t dt = do cur <- b
                       let t' = (prev - cur * dt)  + t
                       dt <- deltaTime time
                       e <- plan (loop cur t' <$> dt)
                       return (pure t' `switch` e)


        


record2 :: Eq a => Behavior Time -> Behavior a -> Duration -> Behavior (Stream (History a))
record2 time b d = b >>= histories
 where samples = ((,) <$> time) `fmapB` fromChanges b
       addNext h (t,s) = fromTime (t - d) (addSample h (t,s))
       histories i = scanlEv addNext (i,empty) samples

type Clock = Behavior Time


          

record :: Behavior Time -> Behavior a -> Duration -> Behavior (Stream (History a))
record time b d = b >>= histories
  where samples = ((\x y -> (y,x)) <$> b) `fmapB` (fromChanges time)
        addNext h (t,s) = fromTime (t - d) (addSample h (t,s))
        histories i = scanlEv addNext (i,empty) samples

delayBy :: Eq a=> Behavior Time -> Behavior a -> Duration -> Behavior (Behavior a)
delayBy time b d = do bufs <- record time b d
                      a <- b
                      let pastVals = fmap (\(i,_) -> pure i) bufs
                      e <- foldr1Ev switch pastVals
                      return (pure a `switch` e)
