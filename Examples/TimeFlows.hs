{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import qualified Graphics.UI.SDL as SDL
import Control.Monad.Fix
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import FRPNow 
import Examples.SDLFRP
import Debug.Trace
import Data.Maybe
import Data.Set hiding (filter,fold, foldl,map)
import qualified Data.Set as S
import Prelude hiding (until)
import Graphics.UI.SDL.Time

nrBoxes = 20
timeDelay = 0.1 -- seconds

-- todo : interpolate boxes

main = do screen <- initSDL
          runNow $
              do evs <- getEvents 
                 clock <- getClock (1.0 / 60.0)
                 mousePos <- sample $ toMousePos evs
                 buttons  <- sample $ toMouseButtonsDown evs
                 bxs <- sample (timeflows nrBoxes timeDelay clock mousePos buttons)
                 drawAll screen bxs
                 sample (quitEv evs)


iteratenM :: Monad m => (a -> m a) -> a -> Integer -> m [a]
iteratenM f a n 
  | n <= 0 = return []
  | otherwise = do h <- f a  
                   t <- iteratenM f h (n - 1)
                   return (h : t)


                              
                   
          
timeflows :: Integer -> Double -> Behavior Time -> Behavior Point -> Behavior (Set MouseBtn) -> Behavior (Behavior [Box])
timeflows n d clock mousePos buttons = 
 do l <- iteratenM nextBox box n
    return $ reverse <$> fadeOut <$> foldl (\x y -> (:) <$> y <*> x) (pure []) (box : l)

  where
  nextBox :: Behavior Box -> Behavior (Behavior Box)
  nextBox b = delayBy clock b d

  fadeOut l = zipWith fade [0..] l where
    fac = 1.0 / fromIntegral n
    fade i (Box r c) = Box r (mixi (i * fac) c black)

  box :: Behavior Box
  box = Box <$> (rectAt (50,50) <$> mousePos) <*> (btnsToColor <$> buttons)



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



