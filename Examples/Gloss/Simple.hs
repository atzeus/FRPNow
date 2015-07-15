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

main = runNowGlossPure (InWindow "FRPNow Gloss!" (800,600) (10,10)) white 60 mainFRP

lerpColor :: Float -> Color -> Color -> Color
lerpColor d ca cb = mixColors d (1-d) ca cb

mainFRP :: Behavior Float -> EvStream GEvent -> Behavior (Behavior Picture)
mainFRP time evs = 
   let posSin x = (sin x + 1) / 2.0
       pict time (x,y) = Color red $ Translate x y (ThickCircle 25 ((posSin time) * 50))
   in do mousePos <- sample (toMousePos evs)
         return (pict <$> time <*> mousePos)

