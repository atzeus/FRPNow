{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import qualified Graphics.UI.SDL as SDL
import Control.Monad.Fix
import Graphics.UI.SDL.Keysym
import Control.Applicative hiding (empty)
import Control.Concurrent
import Control.Monad hiding (when)
import Control.FRPNow 
import Debug.Trace
import Data.Maybe
import Data.Set hiding (filter,fold, foldl,map)
import Prelude hiding (until)

(>$<) = flip (<$>)

main = do screen <- initSDL
          runNow $
              do (evs,quit) <- runEventStreamM <$> open getEvents
                 mousePos <- cur $ toMousePos evs
                 buttons  <- cur $ toMouseButtonsDown evs
                 bxs <- cur (boxes mousePos buttons)
                 drawAll screen bxs
                 return quit


filterUp n@(SDL.MouseButtonDown _ _ m) = Just n
filterUp _ = Nothing

          
boxes :: Behaviour Point -> Behaviour (Set MouseBtn) -> Behaviour (Behaviour [Box])
boxes mousePos buttons = parList $ box `sampleOn` clicks MLeft 
  where
  box :: Behaviour (BehaviourEnd Box ())
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


  dragRect :: Rect -> Behaviour (Behaviour Rect)  
  dragRect r =  behaviour <$> open (loop r) where
    loop r = do pure r `until` clickOn (pure r) MMiddle
                offset <- cur mouseOffset
                let mr = moveRect r <$> offset
                mr `until` release MMiddle
                cur mr >>= loop
                        
  mouseOffset :: Behaviour (Behaviour Point)
  mouseOffset = do p <- cur mousePos
                   return (mousePos >$< (.- p))
   
  clickOn :: Behaviour Rect -> MouseBtn -> Behaviour (Event ())
  clickOn r b = next $ clicks b `during` mouseOver r

  mouseOver :: Behaviour Rect -> Behaviour Bool
  mouseOver r = isInside <$> mousePos <*> r

  clicks :: MouseBtn -> EventStream ()
  clicks m   = repeatEv $ click m 
  releases m = repeatEv $ release m
  click m   = becomesTrue $ isDown m
  release m = becomesTrue $ not <$> isDown m
  isDown m  = (m `member`) <$> buttons


toMouseButtonsDown :: EventStream SDL.Event -> Behaviour (Behaviour (Set MouseBtn))
toMouseButtonsDown = fold updateSet empty where
  updateSet s (SDL.MouseButtonDown _ _ m) | Just m' <- toM m = insert m' s
  updateSet s (SDL.MouseButtonUp   _ _ m) | Just m' <- toM m = delete m' s
  updateSet s _                                              = s

toM SDL.ButtonLeft    = Just MLeft
toM SDL.ButtonMiddle  = Just MMiddle
toM SDL.ButtonRight   = Just MRight
toM _             = Nothing

toMousePos :: EventStream SDL.Event -> Behaviour (Behaviour Point)
toMousePos = fold getMousePos (0.0,0.0)

getMousePos p (SDL.MouseMotion x y _ _) = (fromIntegral x, fromIntegral y)
getMousePos p _                         = p

getEvents ::  (Now :. EventStreamM SDL.Event) ()
getEvents = loop where
 loop = 
  do l <- waitIO ioGetEvents
     if filter (== SDL.Quit) l /= []
     then return ()
     else (mapM_ emit l) >> loop





drawAll :: SDL.Surface -> Behaviour [Box] -> Now ()
drawAll screen b = loop where
  loop :: Now ()
  loop =
   do v <- cur b
      e <- cur $ change b
      e' <- asyncIO $ drawBoxes screen v
      planIO (fmap (const loop) (e >> e'))
      return ()
      


-- nice while thing
again :: Monad m => m Bool
again = return True
stop :: Monad m => m Bool
stop = return False

while :: Monad m => m Bool -> m ()
while m = do v <- m 
             if v 
             then while m
             else return ()

-- Below: IO Stuff

initSDL = do  SDL.init [SDL.InitEverything]
              SDL.setVideoMode 800 600 32 [SDL.DoubleBuf]
              SDL.getVideoSurface


ioGetEvents :: IO [SDL.Event]
ioGetEvents = do h <- SDL.waitEvent
                 t <- loop
                 return (h : t)
  where loop = do h <- SDL.pollEvent 
                  case h of
                    SDL.NoEvent -> return []
                    _       -> do t <- loop ;  return (h : t)

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


data Rect    = Rect {leftup :: Point, rightdown :: Point} deriving (Eq,Show)

moveRect :: Rect -> Point -> Rect
moveRect (Rect lu rd) p = Rect (lu .+ p) (rd .+ p)

isInside :: Point -> Rect -> Bool
isInside (x,y) (normalize -> Rect (l,u) (r,d)) = x >= l && x <= r && y >= u && y <= d

rect p1 p2 = normalize (Rect p1 p2)

toRect :: Rect -> SDL.Rect
toRect (normalize -> Rect (lx,uy) (rx,dy)) = SDL.Rect (round lx) (round uy) (round (rx - lx)) (round (dy - uy))

normalize (Rect (lx,uy) (rx,dy)) = Rect (min lx rx, min uy dy) (max lx rx, max uy dy)

data Color   = Color {  r :: Double, g :: Double, b :: Double} deriving (Eq,Show)

red = Color 1 0 0 
green = Color 0 1 0
blue = Color 0 0 1

getColor :: SDL.Surface -> Color -> IO SDL.Pixel
getColor s c = 
     let fmt = SDL.surfaceGetPixelFormat s in
     SDL.mapRGB fmt (con r) (con g) (con b)
  where con d = round ( (d c) * 255.0)

data Box     = Box Rect Color deriving (Eq, Show)

