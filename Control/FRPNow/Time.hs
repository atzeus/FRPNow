{-# LANGUAGE TupleSections,TypeOperators,MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.FRPNow.Time
-- Copyright   :  (c) Atze van der Ploeg 2015
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
-- 
-- Various utility functions for FRPNow related to the passing of time.
-- All take a "clock" as an argument, i.e. a behavior that
-- gives the seconds since the program started.
--
-- The clock itself is created by a function specialized to the
-- GUI library you are using FRP with such as 'Control.FRPNow.GTK.getClock'

module Control.FRPNow.Time(localTime,timeFrac, lastInputs, bufferBehavior,delayBy, delayByN, delayTime, integrate, VectorSpace(..)) where

import Control.FRPNow.Core
import Control.FRPNow.Lib
import Control.FRPNow.EvStream
import Data.Sequence
import Control.Applicative hiding (empty)
import Data.Foldable
import Debug.Trace




-- | When sampled at time t, gives the time since time t
localTime :: (Floating time, Ord time) =>  Behavior time -> Behavior (Behavior time)
localTime t = do n <- t
                 return ((\x -> x - n) <$> t)

-- | Gives a behavior that linearly increases from 0 to 1 in the specified duration
timeFrac :: (Floating time, Ord time) =>   Behavior time -> time -> Behavior (Behavior time)
timeFrac t d = do t' <- localTime t
                  e <- when $ (>= d) <$> t'
                  let frac = (\x -> min 1.0 (x / d)) <$> t'
                  return (frac `switch` (pure 1.0 <$ e))


-- | Tag the events in a stream with their time
tagTime :: (Floating time, Ord time) => Behavior time  -> EvStream a -> EvStream (time,a)
tagTime c s = ((,) <$> c) <@@> s

-- | Gives a behavior containing the values of the events in the stream that occurred in the last n seconds
lastInputs :: (Floating time, Ord time) =>
    Behavior time -- ^ The "clock" behavior, the behavior monotonically increases with time
    -> time -- ^ The duration of the history to be kept
    -> EvStream a -- ^ The input stream
    -> Behavior (Behavior [a])
lastInputs clock dur s = do s' <- bufferStream clock dur s
                            bs <- fromChanges [] s'
                            let dropIt cur s = dropWhile (\(t,_) -> t + dur < cur) s
                            return $ (fmap snd) <$> (dropIt <$> clock <*> bs)

bufferStream :: (Floating time, Ord time)  => Behavior time  -> time -> EvStream a ->  Behavior (EvStream [(time,a)])
bufferStream clock dur s = do s' <- scanlEv addDrop empty $ tagTime clock s
                              return $  toList <$> s' where
  addDrop ss s@(last,v) = dropWhileL (\(tn,_) -> tn + dur < last) (ss |> s)


data TimeTag t a = TimeTag t a

instance Eq t => Eq (TimeTag t a) where
  (TimeTag t1 _) == (TimeTag t2 _) = t1 == t2



-- | Gives a behavior containing the values of the behavior during the last n seconds, with time stamps
bufferBehavior :: (Floating time, Ord time)  =>
      Behavior time   -- ^ The "clock" behavior, the behavior monotonically increases with time
      -> time -- ^ The duration of the history to be kept
      -> Behavior a  -- ^ The input behavior
      ->  Behavior (Behavior [(time,a)])
bufferBehavior clock dur b = fmap toList <$> foldB update empty (TimeTag <$> clock <*> b)
     where update l (TimeTag now x) = trimList (l |> (now,x)) (now - dur)
           trimList l after = loop l where
             loop l = 
               case viewl l of
                EmptyL -> empty
                (t1,v1) :< tail1 
                   | after <= t1 -> l 
                   | otherwise   -> 
                     case viewl tail1 of
                      (t2,v2) :< tail2 
                          | t2 <= after -> loop tail2
                          | otherwise   -> l


-- | Give a version of the behavior delayed by n seconds
delayBy ::  (Floating time, Ord time)  =>
       Behavior time  -- ^ The "clock" behavior, the behavior monotonically increases with time
       -> time  -- ^ The duration of the delay
       -> Behavior a -- ^ The input behavior
       -> Behavior (Behavior a)
delayBy time d b = fmap (snd . head) <$> bufferBehavior time d b


-- | Give n delayed versions of the behavior, each with the given duration in delay between them. 
delayByN :: (Floating time, Ord time)  => 
         Behavior time  -- ^ The "clock" behavior, the behavior monotonically increases with time
         -> time  -- ^ The duration _between_ delayed versions
         -> Integer -- ^ The number of delayed versions 
         -> Behavior a  -- ^ The input behavior
         -> Behavior (Behavior [a])
delayByN clock dur n b =
  let durN = (fromIntegral n) * dur
  in do samples <- bufferBehavior clock durN b
        return $ interpolateFromList <$> clock <*> samples where
  interpolateFromList now l= loop (n - 1) l where
        loop n l = 
           if n < 0 then []
           else let sampleTime = now - (fromIntegral n * dur)
                in case l of
                   [] -> []
                   [(_,v)] -> v : loop (n-1) l
                   ((t1,v1) : (t2,v2) : rest)
                    | sampleTime >= t2 -> loop n ((t2,v2) : rest)
                    | otherwise -> v1 : loop (n-1) l



-- | Integration using rectangle rule approximation. Integration depends on when we start integrating so the result is @Behavior (Behavior v)@.
integrate :: (VectorSpace v time) => 
             Behavior time -> Behavior v -> Behavior (Behavior v)
integrate time v = do t <- time 
                      vp <- delayTime time (t,zeroVector) ((,) <$> time <*> v)
                      foldB add zeroVector $ (,) <$> vp <*> time
   where add total ((t1,v),t2) = total ^+^ ((t2 - t1) *^ v)


-- | Delay a behavior by one tick of the clock. Occasionally useful to prevent immediate feedback loops. Like 'Control.FRPNow.EvStream.delay', but uses the changes of the clock as an event stream.
delayTime ::  Eq time  =>  Behavior time -> a -> Behavior a -> Behavior (Behavior a)
delayTime time i b = loop i where
  loop i =
           do e <- futuristic $ 
                        do (t,cur) <- (,) <$> time <*> b
                           e <- when ((/= t) <$> time)
                           return (cur <$ e)
              e' <- plan ( loop <$> e)
              return (i `step` e')

infixr *^
infixl ^/
infix 7 `dot`
infixl 6 ^+^, ^-^

-- | A type class for vector spaces. Stolen from Yampa. Thanks Henrik :)

-- Minimal instance: zeroVector, (*^), (^+^), dot
class (Eq a, Eq v, Ord v, Ord a, Floating a) => VectorSpace v a | v -> a where
    zeroVector   :: v
    (*^)         :: a -> v -> v
    (^/)         :: v -> a -> v
    negateVector :: v -> v
    (^+^)        :: v -> v -> v
    (^-^)        :: v -> v -> v
    dot          :: v -> v -> a
    norm         :: v -> a
    normalize    :: v -> v

    v ^/ a = (1/a) *^ v

    negateVector v = (-1) *^ v

    v1 ^-^ v2 = v1 ^+^ negateVector v2

    norm v = sqrt (v `dot` v)

    normalize v = if nv /= 0 then v ^/ nv else error "normalize: zero vector"
        where nv = norm v

------------------------------------------------------------------------------
-- Vector space instances for Float and Double
------------------------------------------------------------------------------

instance VectorSpace Float Float where
    zeroVector = 0

    a *^ x = a * x

    x ^/ a = x / a

    negateVector x = (-x)

    x1 ^+^ x2 = x1 + x2

    x1 ^-^ x2 = x1 - x2

    x1 `dot` x2 = x1 * x2


instance VectorSpace Double Double where
    zeroVector = 0

    a *^ x = a * x

    x ^/ a = x / a

    negateVector x = (-x)

    x1 ^+^ x2 = x1 + x2

    x1 ^-^ x2 = x1 - x2

    x1 `dot` x2 = x1 * x2


------------------------------------------------------------------------------
-- Vector space instances for small tuples of Floating
------------------------------------------------------------------------------

instance (Eq a, Floating a, Ord a) => VectorSpace (a,a) a where
    zeroVector = (0,0)

    a *^ (x,y) = (a * x, a * y)

    (x,y) ^/ a = (x / a, y / a)

    negateVector (x,y) = (-x, -y)

    (x1,y1) ^+^ (x2,y2) = (x1 + x2, y1 + y2)

    (x1,y1) ^-^ (x2,y2) = (x1 - x2, y1 - y2)

    (x1,y1) `dot` (x2,y2) = x1 * x2 + y1 * y2


instance (Eq a, Floating a, Ord a) => VectorSpace (a,a,a) a where
    zeroVector = (0,0,0)

    a *^ (x,y,z) = (a * x, a * y, a * z)

    (x,y,z) ^/ a = (x / a, y / a, z / a)

    negateVector (x,y,z) = (-x, -y, -z)

    (x1,y1,z1) ^+^ (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)

    (x1,y1,z1) ^-^ (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)

    (x1,y1,z1) `dot` (x2,y2,z2) = x1 * x2 + y1 * y2 + z1 * z2


instance (Eq a, Floating a, Ord a) => VectorSpace (a,a,a,a) a where
    zeroVector = (0,0,0,0)

    a *^ (x,y,z,u) = (a * x, a * y, a * z, a * u)

    (x,y,z,u) ^/ a = (x / a, y / a, z / a, u / a)

    negateVector (x,y,z,u) = (-x, -y, -z, -u)

    (x1,y1,z1,u1) ^+^ (x2,y2,z2,u2) = (x1+x2, y1+y2, z1+z2, u1+u2)

    (x1,y1,z1,u1) ^-^ (x2,y2,z2,u2) = (x1-x2, y1-y2, z1-z2, u1-u2)

    (x1,y1,z1,u1) `dot` (x2,y2,z2,u2) = x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2


instance (Eq a, Floating a, Ord a) => VectorSpace (a,a,a,a,a) a where
    zeroVector = (0,0,0,0,0)

    a *^ (x,y,z,u,v) = (a * x, a * y, a * z, a * u, a * v)

    (x,y,z,u,v) ^/ a = (x / a, y / a, z / a, u / a, v / a)

    negateVector (x,y,z,u,v) = (-x, -y, -z, -u, -v)

    (x1,y1,z1,u1,v1) ^+^ (x2,y2,z2,u2,v2) = (x1+x2, y1+y2, z1+z2, u1+u2, v1+v2)

    (x1,y1,z1,u1,v1) ^-^ (x2,y2,z2,u2,v2) = (x1-x2, y1-y2, z1-z2, u1-u2, v1-v2)

    (x1,y1,z1,u1,v1) `dot` (x2,y2,z2,u2,v2) =
        x1 * x2 + y1 * y2 + z1 * z2 + u1 * u2 + v1 * v2

