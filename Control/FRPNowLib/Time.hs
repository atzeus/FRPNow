{-# LANGUAGE TypeOperators #-}

module Control.FRPNowLib.Time where

import Control.FRPNowImpl.FRPNow
import Control.FRPNowLib.Lib
import Control.FRPNowLib.EventStream
import Control.Monad.Swap
import Data.Sequence
import Control.Applicative hiding (empty)
import Data.Time.Clock.POSIX
import Control.Concurrent
import Data.Foldable

-- seconds
type Time = Double
type Duration = Double

type Sample a = (Time,a)
type History a = (a, Seq (Time,a))

getElapsedTimeSeconds :: IO Time
getElapsedTimeSeconds =  fromRational . toRational <$> getPOSIXTime

waitSeconds :: Duration -> IO ()
waitSeconds d = threadDelay (round (d * 1000000)) 

getClock :: Duration -> Now (Behavior Time)
getClock minDelta = loop where
 loop = 
   do now <- syncIO $ getElapsedTimeSeconds 
      e <- asyncIO (waitSeconds minDelta)
      e' <- planIO (loop <$ e)
      return (pure now `switch` e')

type BehaviorT = Reader (Behavior Time) :. Behavior 
type NowT      = Reader (Behavior Time) :. Behavior 


addSample :: History a -> Sample a -> History a
addSample (i,ss) s = (i, ss |> s)


afterTime :: Time -> History a  -> History a
afterTime t (a,s) = 
 case viewl s of
   EmptyL -> (a, empty)
   (ts,x) :< f -> if ts <= t 
                  then afterTime t (x,f) 
                  else (a,s)

deltaTime :: Behavior Time -> Behavior (Event Time)
deltaTime time = do now <- time
                    ((\x -> x - now) <$>) <$> change time 


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

buffer :: Behavior Time -> Duration -> EventStream a -> Behavior (EventStream [a])
buffer time d s = do evs <- scanlEv addDrop empty times
                     return (map snd . toList <$> evs)
  where times = fmapB ((,) <$> time) s
        addDrop l (t,s) = dropBefore (t - d) (l |> (t,s))
        dropBefore :: Time -> Seq (Time,a) -> Seq (Time,a)
        dropBefore t l = 
          case viewl l of
            EmptyL -> empty
            (ts,a) :< tl | ts < t    -> dropBefore t tl
                         | otherwise -> l
                         


record :: Behavior Time -> Behavior a -> Duration -> Behavior (EventStream (History a))
record time b d = b >>= histories
  where samples = ((\x y -> (y,x)) <$> b) `fmapB` (changes time)
        addNext h (t,s) = afterTime (t - d) (addSample h (t,s))
        histories i = scanlEv addNext (i,empty) samples

{-
bufferStream :: Behavior Time -> Duration -> EventStream a -> Behavior (EventStream (Sample a))
bufferStream time maxn  b d = b >>= histories
  where samples = ((\x y -> (y,x)) <$> b) `fmapB` (changes time)
        addNext h (t,s) = afterTime (t - d) (addSample h (t,s))
        histories i = scanlEv addNext (i,empty) samples
-}
delayBy :: Behavior Time -> Behavior a -> Duration -> Behavior (Behavior a)
delayBy time b d = do bufs <- record time b d
                      a <- b
                      let pastVals = fmap (\(i,_) -> pure i) bufs
                      e <- foldr1Ev switch pastVals
                      return (pure a `switch` e)

                
     
     
     

