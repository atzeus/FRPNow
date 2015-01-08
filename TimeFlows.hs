{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import qualified Graphics.UI.SDL as SDL
import Control.Monad.Fix
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import Control.FRPNow 
import SDLFRP
import Debug.Trace
import Data.Maybe
import Data.Set hiding (filter,fold, foldl,map)
import qualified Data.Set as S
import Prelude hiding (until)
import Graphics.UI.SDL.Time
import Control.Concurrent -- for threaddelay


main = do screen <- initSDL
          runNow $
              do evs <- getEvents
                 mousePos <- cur $ toMousePos evs
                 buttons  <- cur $ toMouseButtonsDown evs
                 clock <- makeClock (1.0 / 60.0)
                 bxs <- cur (timeflows 4 0.2 clock mousePos buttons)
                 drawAll screen bxs
                 return never

-- in seconds
makeClock :: Duration -> Now (Behavior Time)
makeClock minDelay = loop  where
  delay = round (minDelay * 1000000.0)
  loop  = 
      do now <- syncIO $ getTicks 
         e <- async (threadDelay delay)
         let nowsecond = fromIntegral now / 1000.0
         e' <- planIO (loop <$ e)
         return (pure nowsecond `switch` e')

iteratenM :: Monad m => (a -> m a) -> a -> Integer -> m [a]
iteratenM f a n 
  | n <= 0 = return []
  | otherwise = do h <- f a 
                   t <- iteratenM f h (n - 1)
                   return (h : t)

          
timeflows :: Integer -> Double -> Behavior Time -> Behavior Point -> Behavior (Set MouseBtn) -> Behavior (Behavior [Box])
timeflows n d clock mousePos buttons = 
 do l <- iteratenM nextBox box n
    return $ foldl (\x y -> (:) <$> y <*> x) (pure []) (box : l)

  where
  nextBox :: Behavior Box -> Behavior (Behavior Box)
  nextBox b = delayBy clock b d

  box :: Behavior Box
  box = Box <$> (rectAt (50,50) <$> mousePos) <*> (btnsToColor <$> buttons)



  btnToColor :: MouseBtn -> Color
  btnToColor MLeft = red
  btnToColor MMiddle = green
  btnToColor MRight = blue
  btnsToColor :: Set MouseBtn -> Color
  btnsToColor s = S.foldl (mixi (1.0 / n )) grey (S.map btnToColor s)
      where n = fromIntegral $ S.size s
  rectAt s p = Rect (p .- hs) (p .+ hs)
        where hs = 0.5 .* s



