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

-- Example from the original FRP paper ("Functional Reactive Animation", Elliot and Hudak)
-- 

main = runNowGlossPure (InWindow "FRPNow Gloss!" (800,600) (10,10)) white 60 mainFRP

lerpColor :: Float -> Color -> Color -> Color
lerpColor d ca cb = mixColors d (1-d) cb ca

mainFRP :: Behavior Float -> EvStream GEvent -> Behavior (Behavior Picture)
mainFRP time evs = 
   let pict speed (x,y) = Color (lerpColor (speed / 1000) green red) $
                          Translate x y $ ThickCircle 25 (50)
   in do mousePos <- toMousePos evs
         (followPos, followSpeed) <- followMouse time mousePos
         return (pict <$> followSpeed <*> followPos)


followMouse :: Behavior Float -> -- The time
               Behavior Point -> -- The mouse
               Behavior (Behavior Point, Behavior Float) -- the result
followMouse time mouse = 
  mdo let dragfactor = 0.1
      let speedup = 10.0
      let accelfunc m p r = speedup *^ ((m ^-^ p) ^-^ (dragfactor *^ r)) 
      let accel = accelfunc <$> mouse <*> pos <*> speed  
      speed <- integrate time accel
      pos   <- integrate time speed
      return (pos,norm <$> speed)
