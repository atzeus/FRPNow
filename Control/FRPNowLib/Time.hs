module Control.FRPNowLib.Time where

import Control.FRPNowImpl.FRPNow
import Control.FRPNowLib.Lib
import Control.FRPNowLib.EventStream
import Data.Sequence
import Control.Applicative hiding (empty)


type Time = Double
type Duration = Double

type Sample a = (Time,a)
type History a = (a, Seq (Time,a))

addSample :: History a -> Sample a -> History a
addSample (i,ss ) s = (i, ss |> s)


afterTime :: Time -> History a  -> History a
afterTime t (a,s) = 
 case viewl s of
   EmptyL -> (a, empty)
   (ts,x) :< f -> if ts <= t 
                  then afterTime t (x,f) 
                  else (a,s)


buffer :: Behavior Time -> Behavior a -> Duration -> Behavior (EventStream (History a))
buffer time b d = b >>= histories
  where samples = ((\x y -> (y,x)) <$> b) `fmapB` (changes time)
        addNext h (t,s) = afterTime (t - d) (addSample h (t,s))
        histories i = scanlEv addNext (i,empty) samples

delayBy :: Behavior Time -> Behavior a -> Duration -> Behavior (Behavior a)
delayBy time b d = do bufs <- buffer time b d
                      a <- b
                      let pastVals = fmap (\(i,_) -> pure i) bufs
                      e <- foldr1Ev switch pastVals
                      return (pure a `switch` e)

                
     
     
     

