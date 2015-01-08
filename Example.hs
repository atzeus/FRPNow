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
import Prelude hiding (until)

(>$<) = flip (<$>)

main = do screen <- initSDL
          runNow $
              do evs <- getEvents
                 mousePos <- cur $ toMousePos evs
                 buttons  <- cur $ toMouseButtonsDown evs
                 bxs <- cur (boxes mousePos buttons)
                 drawAll screen bxs
                 return never


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


  dragRect :: Rect -> Behavior (Behavior Rect)  
  dragRect r =  behavior <$> open (loop r) where
    loop r = do pure r `until` clickOn (pure r) MMiddle
                offset <- cur mouseOffset
                let mr = moveRect r <$> offset
                mr `until` release MMiddle
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



