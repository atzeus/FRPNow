{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import qualified Graphics.UI.SDL as SDL
import Control.Monad.Fix
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import FRPNow 
import Lib.Lib
import Lib.EventStream
import Examples.SDLFRP
import Debug.Trace
import Data.Maybe
import Data.Set hiding (filter,fold, foldl,map)
import Prelude hiding (until)


main = do screen <- initSDL
          runFRP $
              do evs <- getEvents
                 -- mousePos <- cur $ toMousePos evs
                 {-buttons  <- cur $ toMouseButtonsDown evs
                 bxs <- cur (boxes mousePos buttons)
                 drawAll screen bxs
                 -}
                 showChanges (fromChanges SDL.NoEvent evs) 
                 --printAll evs
                 return never


getMouse ::  Now (Behavior [SDL.Event])
getMouse = loop [] where
  loop l =  do e <- async ioWaitEvents
               e' <- plan (loop <$> e)
               return (l `step` e')
               

{-getEm ::  Now ()
getEm = 
  loop =  do e <-  async ioWaitEvents
             plan (loop <$ e)
-}

{-
boxes :: Behavior Point -> Behavior (Set MouseBtn) -> Behavior (Behavior [Box])
boxes mousePos buttons = parList $ box `sampleOn` clicks MLeft 
  where
  box :: Behavior (BehaviorEnd Box ())
  box = open $
     do p1 <- cur mousePos
        let defineRect = rect p1 <$> mousePos
        let defineBox = Box <$> defineRect <*> pure red
        defineBox `until` release MLeft
        p2 <- cur mousePos
        --let r = pure $ rect p1 p2
        r  <- cur $ dragRect (rect p1 p2)
        let mo = mouseOver r
        let toColor True  = green
            toColor False = red 
        let color = toColor <$> mo
        
        (Box  <$> r <*> color)  `until`  clickOn r MRight


  movableRect :: Rect -> Behavior (Behavior Rect)
  movableRect r = -- monadfix
    mdo drag <- isDragging rect
        rect <- dragRect drag r
        return rect

  isDragging :: Behavior Rect -> Behavior (Behavior Bool)
  isDragging rect =  behavior <$> open (forever loop) where
    loop = do pure False `until` next (clicks MMiddle `during` mouseOver)
              pure True `until` when (not <$> mouseOver)

  dragRect :: Rect -> Behavior Bool -> Behavior (Behavior Rect)  
  dragRect isDragging r =  behavior <$> open (loop r) where
    loop r = do pure r `until` when isDragging
                offset <- cur mouseOffset
                let mr = moveRect r <$> offset
                mr `until` when (not <$> isDragging)
                cur mr >>= loop
                        
  mouseOffset :: Behavior (Behavior Point)
  mouseOffset = do p <- cur mousePos
                   return (mousePos >$< (.- p))
   
  clickOn :: Behavior Rect -> MouseBtn -> Behavior (Event ())
  clickOn r b = next $ clicks b `during` mouseOver r

  mouseOver :: Behavior Rect -> Behavior Bool
  mouseOver r = isInside <$> mousePos <*> r

  clicks :: MouseBtn -> EventStream ()
  clicks m   = repeatEv $ click m 
  releases m = repeatEv $ release m
  click m   = becomesTrue $ isDown m
  release m = becomesTrue $ not <$> isDown m
  isDown m  = (m `member`) <$> buttons
-}


