
{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}
module Examples.SDLFRP where

import Examples.SimpleGraphics
import qualified Graphics.UI.SDL as SDL
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import FRPNow
import Debug.Trace

import Data.Set hiding (filter,fold, foldl,map)
import Data.Word

-- IO stuff

initSDL = do  SDL.init [SDL.InitEverything]
              SDL.enableEvent SDL.SDLMouseMotion True
              SDL.setVideoMode 800 600 32 [SDL.DoubleBuf]
              SDL.getVideoSurface


ioWaitEvents :: IO [SDL.Event]
ioWaitEvents = do h <- SDL.waitEventBlocking
                  t <- getEventsIO
                  traceIO (show (h:t))
                  return (h : t)


getEventsIO =
  do h <- SDL.pollEvent
     case h of
       SDL.NoEvent -> return []
       _           -> (h :) <$> getEventsIO

drawBox :: SDL.Surface -> Box -> IO ()
drawBox s (Box r c) =
  do p <- getColor s c
     SDL.fillRect s (Just $ toRect r) p
     return ()

drawBoxes s l =
  do p <- getColor s (Color 0 0 0)
     SDL.fillRect s (Just $ SDL.Rect 0 0 1200 1000) p
     mapM_ (drawBox s) (reverse l)
     SDL.flip s


toRect :: Rect -> SDL.Rect
toRect (R (P lx uy) (P rx dy)) = SDL.Rect (round lx) (round uy) (round (rx - lx)) (round (dy - uy))

getColor :: SDL.Surface -> Color -> IO SDL.Pixel
getColor s c =
     let fmt = SDL.surfaceGetPixelFormat s in
     SDL.mapRGB fmt (con r) (con g) (con b)
  where con d = round ( (d c) * 255.0)



-- FRP Stuff

toMouseButtonsDown :: Stream SDL.Event -> Behavior (Behavior (Set MouseBtn))
toMouseButtonsDown = fold updateSet empty where
  updateSet s (SDL.MouseButtonDown _ _ m) | Just m' <- toM m = insert m' s
  updateSet s (SDL.MouseButtonUp   _ _ m) | Just m' <- toM m = delete m' s
  updateSet s _                                              = s

toM SDL.ButtonLeft    = Just MLeft
toM SDL.ButtonMiddle  = Just MMiddle
toM SDL.ButtonRight   = Just MRight
toM _             = Nothing

toMousePos :: Stream SDL.Event -> Behavior (Behavior Point)
toMousePos = fold getMousePos (0.0,0.0)

getMousePos p (SDL.MouseMotion x y _ _) = P (fromIntegral x) (fromIntegral y)
getMousePos p _                         = p

drawAll :: SDL.Surface -> Behavior [Box] -> Now ()
drawAll screen b = loop where
  loop :: Now ()
  loop =
   do v <- sample b
      e <- sample $ change b
      e' <- async $ drawBoxes screen v
      plan (loop <$ (e >> e'))
      return ()

getEvents ::  Now (Stream SDL.Event )
getEvents = repeatIOList ioWaitEvents

quitEv :: Stream SDL.Event -> Behavior (Event ())
quitEv s = (() <$) <$> (next $ filterStream (SDL.Quit ==) s)
