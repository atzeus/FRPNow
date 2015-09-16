{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import Control.Monad.Fix
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import Control.FRPNow

import Control.FRPNow.Gloss
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Interface.IO.Game hiding (Event)
import Graphics.Gloss
import Debug.Trace
import Data.Maybe
import qualified Data.Set as Set
import Data.Set hiding (filter,fold, foldl,map)
import Prelude hiding (until)

{- Very simple drawing program showing off some advanced FRP constructions, namely BehaviorEnd and parlist

   hold down the left mouse button to draw a box
   delete a box by clicking on it with the right mouse button
   drag a box with the middle mouse button
-}


main = runNowGloss (InWindow "FRPNow Gloss!" (800,600) (10,10)) white 60 mainFRP

lerpColor :: Float -> Color -> Color -> Color
lerpColor d ca cb = mixColors d (1-d) ca cb

mainFRP :: Behavior Float -> EvStream GEvent -> Now (Behavior Picture)
mainFRP time evs = 
  do mousePos <-  sample $ toMousePos evs
     buttons   <- filterMouseButtons <$> sample (toKeysDown evs)
     --traceChanges "mouse: " mousePos
     sample (boxes mousePos buttons)


(.+) :: Point -> Point -> Point
(x,y) .+ (x',y') = (x + x', y + y')

(.-) :: Point -> Point -> Point
(x,y) .- (x',y') = (x - x', y - y')



data Rect = Rect { leftup :: Point , rightdown :: Point }

isInside p (Rect c1 c2) = pointInBox p c1 c2

moveRect (Rect p1 p2) m = Rect (p1 .+ m) (p2 .+ m)

drawRect :: Rect -> Picture
drawRect (Rect (xl,yu) (xr,yd)) = Polygon [(xl,yu),(xr,yu),(xr,yd),(xl,yd)]

boxes :: Behavior Point -> Behavior (Set MouseButton) -> Behavior (Behavior Picture)
boxes mousePos buttons = do boxes <- parList ( box `snapshots` clicks LeftButton )
                            return (Pictures . reverse <$> boxes) where
  box :: Behavior (BehaviorEnd Picture ())
  box = open $
     do p1 <- sample mousePos
        let defineRect = Rect p1 <$> mousePos
        let defineBox = Color <$> pure red <*> (drawRect <$> defineRect)
        defineBox `till` next (releases LeftButton)
        r  <- sample $ defineRect
        r  <- sample $ movableRect r
        let mo = mouseOver r
        let toColor True  = green
            toColor False = red
        let color = toColor <$> mo
        (Color  <$> color <*> (drawRect <$> r))  `till`  clickOn r RightButton


  movableRect :: Rect -> Behavior (Behavior Rect)
  movableRect r = behavior <$> open (loop r) where
    loop :: Rect -> (Behavior :. BehaviorEnd Rect) ()
    loop r = do let rb = pure r
                rb `till` next (clicks MiddleButton `during` mouseOver rb)
                off <- sample mouseOffset
                let r' = (r `moveRect`) <$> off
                r' `till` release MiddleButton
                sample r' >>= loop

  mouseOffset :: Behavior (Behavior Point)
  mouseOffset = do p <- mousePos
                   return ((.- p) <$> mousePos)

  clickOn :: Behavior Rect -> MouseButton -> Behavior (Event ())
  clickOn r b = next $ clicks b `during` mouseOver r

  mouseOver :: Behavior Rect -> Behavior Bool
  mouseOver r = isInside <$> mousePos <*> r

  clicks :: MouseButton	-> EvStream ()
  clicks m   = edges $ (m `Set.member`) <$> buttons
  releases m = edges $ not . (m `Set.member`) <$> buttons
  release m = next (releases m)
  

