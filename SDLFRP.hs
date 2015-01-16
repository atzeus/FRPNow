
{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}
module SDLFRP where

import qualified Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Keysym
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import Control.FRPNow 
import Data.Set hiding (filter,fold, foldl,map)
import Data.Word

initSDL = do  SDL.init [SDL.InitEverything]
              SDL.setVideoMode 800 600 32 [SDL.DoubleBuf]
              SDL.getVideoSurface


ioWaitEvents :: IO [SDL.Event]
ioWaitEvents = do h <- SDL.waitEvent
                  t <- getEventsIO
                  return (h : t)


getEventsIO = 
  do h <- SDL.pollEvent 
     case h of
       SDL.NoEvent -> return []
       _       -> do t <- getEventsIO ;  return (h : t)

waitEventTimeout :: Word32 -> IO ()
waitEventTimeout w = 
 do h <- SDL.pollEvent 
    case h of
      SDL.NoEvent -> do now <- SDL.getTicks
                        if now >= w
                        then return ()
                        else waitEventTimeout w
      _ -> SDL.pushEvent h >> return ()


ioGetEventsTimeout :: Word32 -> IO [SDL.Event] 
ioGetEventsTimeout w = 
   do now <- SDL.getTicks
      waitEventTimeout (now + w)
      evs <- getEventsIO
      return evs

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


toMouseButtonsDown :: EventStream SDL.Event -> Behavior (Behavior (Set MouseBtn))
toMouseButtonsDown = fold updateSet empty where
  updateSet s (SDL.MouseButtonDown _ _ m) | Just m' <- toM m = insert m' s
  updateSet s (SDL.MouseButtonUp   _ _ m) | Just m' <- toM m = delete m' s
  updateSet s _                                              = s

toM SDL.ButtonLeft    = Just MLeft
toM SDL.ButtonMiddle  = Just MMiddle
toM SDL.ButtonRight   = Just MRight
toM _             = Nothing

toMousePos :: EventStream SDL.Event -> Behavior (Behavior Point)
toMousePos = fold getMousePos (0.0,0.0)

getMousePos p (SDL.MouseMotion x y _ _) = (fromIntegral x, fromIntegral y)
getMousePos p _                         = p

getEvents ::  Now (EventStream SDL.Event )
getEvents = Es <$> loop where
  loop = do e <-  asyncOS (ioWaitEvents)
            e' <- planIO (loop <$ e)
            return (pure e `switch` e')

{-
getEventsTime ::  Double -> Now (EventStream SDL.Event , Behavior Time )
getEventsTime delay = 
  do (a,b) <- loop
     return (Es a, b) where
  loop = do e <-  asyncIO (ioGetEventsTimeout delaymillis)
            e' <- planIO (loop <$ e)
            let evs = fst <$> e'
            let timeb = snd <$> e'
            t <- syncIO $ SDL.getTicks
            let ti = fromIntegral t /1000.0
            --syncIO (putStrLn (show ti))
            return (pure e `switch` evs, pure ti `switch` timeb)
  delaymillis = round (delay * 1000.0)
-}




drawAll :: SDL.Surface -> Behavior [Box] -> Now ()
drawAll screen b = loop where
  loop :: Now ()
  loop =
   do v <- cur b
      e <- cur $ change b
      e' <- asyncIO $ drawBoxes screen v
      planIO (fmap (const loop) (e >> e'))
      return ()
      


