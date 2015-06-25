{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import Control.Monad.Fix
import Data.Sequence.BSeq
import Data.Foldable
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import FRPNow
import qualified Graphics.UI.WX as WX
import Examples.WXFRP
import Examples.SimpleGraphics
import Debug.Trace
import Data.Maybe
import Data.Set hiding (filter,fold, foldl,map,empty,toList)
import qualified Data.Set as S
import Prelude hiding (until)

duration = 2 -- seconds

-- todo : interpolate boxes

main =runWx $
            mdo 
                 
                 now <- syncIO getElapsedTimeSeconds
                 clock <- toChanges now ticks
                 --buttons   <- sample $ fold updateSet empty btnEvs
                 mouse <- toChanges (0,0) moveEvs
                 points <- sample $ recordB clock duration mouse
		 (moveEvs, btnEvs,ticks) <- boxWindowTimer "Hullo" 800 600 60 (map (toBox . snd) . toList <$> points)
                 return ()


toBox :: Point -> Box
toBox p = Box red (rectAt (25,25) p)
{-
getHead :: Record Point -> Time -> Point
getHead x@(Record (ta,a) r) ts =  case viewl r of
       EmptyL -> a
       (tb,b) :< _ -> if ts >= ta && tb > ta
                      then  let f = (ts - ta) / (tb - ta)
                            in  lerpPoint f a b 
                      else b


makeCenters ::  Record Point -> Time -> [Point]
makeCenters r t = loop 0 (t - nrBoxes * timeDelay) r where
--   loop :: Integer -> Time -> Record Point -> [Point]
   loop i ts r
       | i > nrBoxes = []
       | otherwise = let r' = trimTillTime r ts
                     in  getHead r' ts : loop (i + 1) (ts + timeDelay) r' 
         

mixi f l r = lerpColor l f r

iteratenM :: Monad m => (a -> m a) -> a -> Integer -> m [a]
iteratenM f a n
  | n <= 0 = return []
  | otherwise = do h <- f a
                   t <- iteratenM f h (n - 1)
                   return (h : t)
-}
rectAt s p = Rect (p .- hs) (p .+ hs)
        where hs = 0.5 .* s
{-
timeflows :: Integer -> Double -> Behavior Time -> Behavior Point -> Behavior (Set MouseBtn) -> Behavior (Behavior [Box])
timeflows n d clock mousePos buttons =
 do l <- iteratenM nextBox box n
    return $ reverse <$> fadeOut <$> foldl (\x y -> (:) <$> y <*> x) (pure []) (box : l)

  where
  nextBox :: Behavior Box -> Behavior (Behavior Box)
  nextBox b = delayBy clock b d

  fadeOut l = zipWith fade [0..] l where
    fac = 1.0 / fromIntegral n
    fade i (Box c r) = Box  (mixi (i * fac) c white) r

  box :: Behavior Box
  box = Box <$> pure red <*> (rectAt (25,25) <$> mousePos)



  btnToColor :: MouseBtn -> Color
  btnToColor MLeft = red
  btnToColor MMiddle = green
  btnToColor MRight = blue
  btnsToColor :: Set MouseBtn -> Color
  btnsToColor s = if n == 0
                  then white
                  else S.foldl (mixi (1.0 / n )) grey (S.map btnToColor s)

      where n = fromIntegral $ S.size s
-}
