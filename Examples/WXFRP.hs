{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}

module Examples.WXFRP where

import qualified Examples.SimpleGraphics as G
import Graphics.UI.WX
import FRPNow
import Data.IORef
import Control.Applicative
import Control.Concurrent


color2Color :: G.Color -> Color
color2Color (G.C r g b) = rgb (toByte r) (toByte g) (toByte b)
  where toByte x = round (x * 255)

rect2Rect :: G.Rect -> Rect 
rect2Rect (G.Rect (xl,yu) (xr,yd)) = Rect (round xl) (round yu) (round (xr -xl)) (round (yd - yu))


boxWindows :: Int -> String -> Double -> Double ->  Behavior [G.Box] -> Now (Stream G.Point, Stream G.BtnAction)
boxWindows n name w h boxes = 
  do streams <- sequence $ replicate n (boxWindow name w h boxes) 
     return (foldl1 (\(xa,ya) (xb,yb) -> (merge xa xb, merge ya yb)) streams)

-- call start before this!
boxWindow :: String -> Double -> Double ->  Behavior [G.Box] -> Now (Stream G.Point, Stream G.BtnAction)
boxWindow name w h boxes = (\(x,y,_) -> (x,y)) <$> boxWindowFrame name w h boxes


boxWindowTimer :: String -> Double -> Double -> Double -> Behavior [G.Box] -> Now (Stream G.Point, Stream G.BtnAction, Stream Time)
boxWindowTimer s w h fps boxes = 
    do (p,b,f) <- boxWindowFrame s w h boxes
       (ti, callTime) <- callbackStream
       syncIO $ timer f [interval := round (1000.0 / fps), on command := getElapsedTimeSeconds >>= callTime]
       return (p,b,ti)



-- call start before this!
boxWindowFrame :: String -> Double -> Double ->  Behavior [G.Box] -> Now (Stream G.Point, Stream G.BtnAction, Frame ())
boxWindowFrame name w h boxes  = 
    do (m, callMouse) <- callbackStream
       r <- syncIO $ newIORef []
       f <- frpFrame [text := name]
       -- create a panel to draw in.
       p <- syncIO $ panel f [on paint := paintBoxes r]
       callSyncIOStream (repaintBoxes p r) (fromChanges boxes)
       -- react on user input
       syncIO $ set p [on mouse         := callMouse ]
  
       -- put the panel in the frame, with a minimal size
       syncIO $ set f [layout := minsize (sz (round  w) (round  h)) $ widget p]
       return (toMousePos m,toMouseBtn m,f)
   where
    paintBoxes :: IORef [G.Box] -> DC a -> Rect -> IO () 
    paintBoxes r dc _ = 
        do  boxes <- readIORef r
            mapM_ (paintBox dc) boxes

    paintBox :: DC a -> G.Box -> IO ()
    paintBox dc (G.Box c r) =
          drawRect dc (rect2Rect r) [brushColor := color2Color c, brushKind := BrushSolid]

    repaintBoxes :: Panel () -> IORef [G.Box] -> [G.Box] -> IO ()
    repaintBoxes p r boxes = 
       do writeIORef r boxes
          repaint p

    toMousePos :: Stream EventMouse -> Stream G.Point
    toMousePos s = fmap toMousePos s where
       toMousePos ev = let Point x y = mousePos ev
                       in (fromIntegral x, fromIntegral y)

    toMouseBtn :: Stream EventMouse -> Stream G.BtnAction
    toMouseBtn s = filterJusts $ fmap conv s where
      conv (MouseLeftDown   _ _) = Just (G.MLeft  , True  )
      conv (MouseLeftUp     _ _) = Just (G.MLeft  , False )
      conv (MouseMiddleDown _ _) = Just (G.MMiddle, True  )
      conv (MouseMiddleUp   _ _) = Just (G.MMiddle, False )                                
      conv (MouseRightDown  _ _) = Just (G.MRight , True  )
      conv (MouseRightUp    _ _) = Just (G.MRight , False )
      conv _                     = Nothing
 

