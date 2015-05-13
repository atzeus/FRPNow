{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import Control.Monad.Fix
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import FRPNow
import qualified Graphics.UI.WX as WX
import Examples.WXFRP
import Examples.SimpleGraphics
import Debug.Trace
import Data.Maybe
import Data.Set hiding (filter,fold, foldl,map)
import qualified Data.Set as S
import Prelude hiding (until)

nrBoxes = 15

-- todo : interpolate boxes

main =runWx $
            mdo  mousePos <- toChanges (0,0) moveEvs
                 buttons   <- sample $ fold updateSet empty btnEvs
                 bxs <- sample (timeflows nrBoxes mousePos buttons)
		 (moveEvs, btnEvs) <- boxWindow "Hullo" 800 600 bxs
                 return ()



iteratenM :: Monad m => (a -> m a) -> a -> Integer -> m [a]
iteratenM f a n
  | n <= 0 = return []
  | otherwise = do h <- f a
                   t <- iteratenM f h (n - 1)
                   return (h : t)

delayDiscreteB :: Eq a => a -> Behavior a -> Behavior (Behavior a)
delayDiscreteB i b = do s <- delayDiscreteN 3 (fromChanges b)
                        asChanges i s

mixi f l r = lerpColor l f r

timeflows :: Integer ->  Behavior Point -> Behavior (Set MouseBtn) -> Behavior (Behavior [Box])
timeflows n mousePos buttons = 
 do l <- iteratenM nextBox box n
    return $ reverse <$> fadeOut <$> foldl (\x y -> (:) <$> y <*> x) (pure []) (box : l)

  where
  nextBox :: Behavior Box -> Behavior (Behavior Box)
  nextBox b = delayDiscreteB (Box red (rectAt (25,25) (0,0))) b

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
  rectAt s p = Rect (p .- hs) (p .+ hs)
        where hs = 0.5 .* s
