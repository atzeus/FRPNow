{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import Control.Monad.Fix
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import Control.FRPNow
import Control.FRPNow.Gloss
import Graphics.Gloss
import Debug.Trace
import Data.Maybe
import Data.Set hiding (filter,fold, foldl,map)
import Prelude hiding (until)

-- Shows of delayByN

nrBoxes = 20
timeDelay = 0.3 -- seconds

width = 800
height = 600

main = runNowGloss (InWindow "FRPNow Gloss!" (width,height) (10,10)) white 120 (\t e -> sample $ mainFRP t e)

mainFRP :: Behavior Float -> EvStream GEvent -> Behavior (Behavior Picture)
mainFRP clock evs = 
      do mouse <- toMousePos evs
         mouseHist <- delayByN clock timeDelay nrBoxes mouse
         let pict (x,y) = Color red $ Translate x y (ThickCircle 10 5)
         return (Pictures . map pict <$> mouseHist)




