
{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}
module Examples.SDLFRP where

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
                  --putStr (show (h:t))
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

-- Points, buttons and boxes

data MouseBtn  = MLeft | MMiddle | MRight deriving (Ord,Eq,Show)

type Point     = (Double,Double) -- in pixels

(.+) :: Point -> Point -> Point
(x,y) .+ (x',y') = (x+x', y + y')

(.-) :: Point -> Point -> Point
(x,y) .- (x',y') = (x-x', y - y')

(.*) :: Double -> Point -> Point
d .* (x,y) = (d * x, d * y)


data Rect    = Rect {leftup :: Point, rightdown :: Point} deriving (Eq,Show)

moveRect :: Rect -> Point -> Rect
moveRect (Rect lu rd) p = Rect (lu .+ p) (rd .+ p)

isInside :: Point -> Rect -> Bool
isInside (x,y) (normalize -> Rect (l,u) (r,d)) = x >= l && x <= r && y >= u && y <= d

rect p1 p2 = normalize (Rect p1 p2)

toRect :: Rect -> SDL.Rect
toRect (normalize -> Rect (lx,uy) (rx,dy)) = SDL.Rect (round lx) (round uy) (round (rx - lx)) (round (dy - uy))

normalize (Rect (lx,uy) (rx,dy)) = Rect (min lx rx, min uy dy) (max lx rx, max uy dy)

data Color   = Color {  r :: Double, g :: Double, b :: Double} deriving (Eq,Show,Ord)

red = Color 1 0 0 
green = Color 0 1 0
blue = Color 0 0 1
black = Color 0 0 0
grey = Color 0.1 0.1 0.1
white = Color 1 1 1 

lerpPoint :: Double -> Point -> Point -> Point
lerpPoint i (x,y) (x',y') = (lerp i x x', lerp i y y')

lerp :: Double -> Double -> Double -> Double
lerp i l r = r * i' + l * (1 - i') where
  i' = min 1.0 (max 0.0 i)

bool2Color True  = green
bool2Color False = red 

mix :: Color -> Color -> Color
mix l r = mixi 0.5 l r 

mixi :: Double -> Color -> Color  -> Color 
mixi i (Color rl gl bl) (Color rr gr br) = Color (lerp rl rr i) (lerp gl gr i) (lerp bl br i) 

getColor :: SDL.Surface -> Color -> IO SDL.Pixel
getColor s c = 
     let fmt = SDL.surfaceGetPixelFormat s in
     SDL.mapRGB fmt (con r) (con g) (con b)
  where con d = round ( (d c) * 255.0)

data Box     = Box Rect Color deriving (Eq, Show)

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

getMousePos p (SDL.MouseMotion x y _ _) = (fromIntegral x, fromIntegral y)
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




      


