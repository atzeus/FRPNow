{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import Control.Monad.Fix
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import Examples.SimpleGraphics
import FRPNow
import Examples.WXFRP
import qualified Graphics.UI.WX as WX
import Examples.SimpleGraphics
import Debug.Trace
import Data.Maybe
import Data.Set hiding (filter,fold, foldl,map)
import Prelude hiding (until)

{- Very simple drawing program
   hold down the left mouse button to draw a box
   delete a box by clicking on it with the right mouse button
   drag a box with the middle mouse button
-}


main = runWx $ trace "Bla" $
            mdo  mousePos <-  toChanges (0,0) moveEvs
                 buttons   <- sample $ fold updateSet empty btnEvs
                 bxs <- sample (boxes mousePos buttons)
		 (moveEvs, btnEvs) <-  boxWindows 2 "Hullo" 800 600 bxs
                 return ()




boxes :: Behavior Point -> Behavior (Set MouseBtn) -> Behavior (Behavior [Box])
boxes mousePos buttons = parList ( box `sampleOn` clicks MLeft )
  where
  box :: Behavior (BehaviorEnd Box ())
  box = open $
     do p1 <- cur mousePos
        let defineRect = rect p1 <$> mousePos
        let defineBox = Box <$> pure red <*> defineRect
        defineBox `until` release MLeft
        r  <- cur $ defineRect
        r  <- cur $ movableRect r
        let mo = mouseOver r
        let toColor True  = green
            toColor False = red
        let color = toColor <$> mo
        (Box  <$> color <*> r)  `until`  clickOn r MRight


  movableRect :: Rect -> Behavior (Behavior Rect)
  movableRect r = behavior <$> open (loop r) where
    loop :: Rect -> (Behavior :. BehaviorEnd Rect) ()
    loop r = do let rb = pure r
                rb `until` next (clicks MMiddle `during` mouseOver rb)
                off <- cur mouseOffset
                let r' = (r `moveRect`) <$> off
                r' `until` release MMiddle
                cur r' >>= loop

  mouseOffset :: Behavior (Behavior Point)
  mouseOffset = do p <- mousePos
                   return ((.- p) <$> mousePos)

  clickOn :: Behavior Rect -> MouseBtn -> Behavior (Event ())
  clickOn r b = next $ clicks b `during` mouseOver r

  mouseOver :: Behavior Rect -> Behavior Bool
  mouseOver r = isInside <$> mousePos <*> r

  clicks :: MouseBtn -> Stream ()
  clicks m   = repeatEv $ click m
  releases m = repeatEv $ release m
  click m   = becomesTrue $ isDown m
  release m = becomesTrue $ not <$> isDown m
  isDown m  = (m `member`) <$> buttons

{-

-}
