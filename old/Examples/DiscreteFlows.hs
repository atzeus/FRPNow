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

nrBoxes = 400

-- todo : interpolate boxes

main =runWx $
            mdo  mousePos <- toChanges (0,0) moveEvs
                 --buttons   <- sample $ fold updateSet empty btnEvs
                 bxs <- sample (timeflows1 mousePos)
		 (moveEvs, btnEvs) <- boxWindow "Hullo" 800 600 bxs
                 return ()



iteratenM :: Monad m => (a -> m a) -> a -> Integer -> m [a]
iteratenM f a n
  | n <= 0 = return []
  | otherwise = do h <- f a
                   t <- iteratenM f h (n - 1)
                   return (h : t)

mixi f l r = lerpColor l f r

rectAt s p = Rect (p .- hs) (p .+ hs)
        where hs = 0.5 .* s

timeflows2 :: Behavior Point -> Behavior (Behavior [Box])
timeflows2 b = do b' <- foldB (\l p -> take nrBoxes (p:l)) [] b
                  return (map (\x -> Box red (rectAt (50,50) x)) <$> b')

timeflows1 :: Behavior Point -> Behavior (Behavior [Box])
timeflows1 b = do b' <- getEm b nrBoxes
                  return (map (\x -> Box red (rectAt (50,50) x)) <$> b')
  where getEm b n 
          | n == 0 = return (pure [])
          | otherwise = do b' <- prev (0,0) b
                           t <- getEm b' (n-1)
                           return $ (:) <$> b <*> t
                  

timeflows :: Integer ->  Behavior Point -> Behavior (Set MouseBtn) -> Behavior (Behavior [Box])
timeflows n mousePos buttons = 
 do l <- iteratenM nextBox box n
    return $ reverse <$> fadeOut <$> foldl (\x y -> (:) <$> y <*> x) (pure []) (box : l)

  where
  nextBox :: Behavior Box -> Behavior (Behavior Box)
  nextBox b = prev (Box red (rectAt (25,25) (0,0))) b

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

