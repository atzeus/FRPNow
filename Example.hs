{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import qualified Graphics.UI.SDL as SDL
import Control.Monad.Fix
import Graphics.UI.SDL.Keysym
import Control.Applicative hiding (empty)
import Control.Concurrent
import Control.Monad hiding (when)
import Base.FRPNow 
import FunctorCompose
import Lib
import EventStream
import Debug.Trace
import Data.Set hiding (filter,fold)
import Prelude hiding (until)

main = do screen <- initSDL
          evs <- stream <$> decomp getEvents
          --bxs <- cur (decomp $ boxes evs)
          --drawAll screen bxs
          printAll evs

          
boxes :: EventStream SDL.Event -> Behaviour2 [Box]
boxes evs = (\x -> [toBox x]) <$> mousePos where
  mousePos = toMousePos evs
  toBox p = Box (normalize $ Rect (100,100) p) red

{-
          runNow (mainFRP screen)
          putStrLn "Thank you for using SuperawesomeDraw 0.1!"
          return ()

mainFRP :: SDL.Surface -> Now s (Event s ()) 
mainFRP screen = 
          do (evs, end) <- runEventStreamM getEvents
             mousePos <- liftB  $ toMousePos evs
             mButsDown <- liftB $ toMouseButtonsDown evs
             b <- liftB $ boxes mousePos mButsDown evs
             runEventM $ drawAll screen b
             return end

boxes :: Behaviour s Point -> Behaviour s (Set MouseBtn) -> EventStream s SDL.Event -> Behaviour s (Behaviour s [Box])
boxes mousePos mButsDown evs = 
   do let es = filterES isLeftClick evs
      let es' = mapES (\(SDL.MouseButtonDown x y _) -> (fromIntegral x, fromIntegral y)) es
      let es2 = mapES (box mousePos mButsDown) es'
      parList es2
 where isLeftClick (SDL.MouseButtonDown _ _ SDL.ButtonRight) = True
       isLeftClick _ = False

box :: forall s. Behaviour s Point -> Behaviour s (Set MouseBtn) -> Point -> Behaviour s (BehaviourEnd s Box ())
box mouse mouseDown p = runUntilM $
  do  Box r _ <- defineBox `untilbl` release MRight mouseDown

      (mouseOver , dragRect) <- liftB $ 
        mdo let mo = mouseOver dr  -- mutually dependent!
            shouldMove <- dragging mo
            dr <- dragRect shouldMove r
            return (mo,dr)
      
      let color   = toColor <$> mouseOver
      let dragBox = Box <$> dragRect <*> color
      dragBox `untilbl` clickWhile mouseOver
      return () 
 where
  toColor True = red
  toColor False = green

  defineBox = Box <$> rectMouse <*> pure red 
  rectMouse = normalize <$> Rect p <$> mouse

  clickWhile mouseOver = 
     do clicks <- repeatEv $ click MMiddle mouseDown
        nextES $ filterES id $ mouseOver `on` clicks
        
  dragging mouseOver = runUntilMl $ forever $ 
       do pure False `untilb` click MLeft mouseDown
          r <- liftB $ mouseOver
          if r
          then pure True `untilB` (not <$> isDown MLeft mouseDown)
          else return ()

  mouseOver rect = isInside <$> mouse <*> rect

  localMouse = do startPos <- liftB $ mouse
                  return $ (.- startPos) <$> mouse

  dragRect shouldMove r = runUntilMl $ loop r where
    loop :: Rect -> UntilM Behaviour Rect s ()
    loop r = do pure r `untilB` shouldMove
                move <- liftB $ localMouse
                let movingRect = moveRect r <$> move
                movingRect `untilB` (not <$> shouldMove)
                r' <- liftB $ movingRect
                loop r'
              
isDown :: MouseBtn -> Behaviour s (Set MouseBtn) -> Behaviour s Bool
isDown m b = member m <$> b
click m b = becomesTrue $ isDown m b
release m b = becomesTrue $ not <$> isDown m b
  
-}
toMouseButtonsDown :: EventStream SDL.Event -> Behaviour2 (Set MouseBtn)
toMouseButtonsDown = fold updateSet empty where
  updateSet s (SDL.MouseButtonDown _ _ m) | Just m' <- toM m = insert m' s
  updateSet s (SDL.MouseButtonUp   _ _ m) | Just m' <- toM m = delete m' s
  updateSet s _                                              = s

toM SDL.ButtonLeft    = Just MLeft
toM SDL.ButtonMiddle  = Just MMiddle
toM SDL.ButtonRight   = Just MRight
toM _             = Nothing

toMousePos :: EventStream SDL.Event -> Behaviour2 Point
toMousePos = fold getMousePos (0.0,0.0)
  where getMousePos p (SDL.MouseMotion x y _ _) = (fromIntegral x, fromIntegral y)
        getMousePos p _                         = p

getEvents ::  (IO :. EventStreamEnd SDL.Event) ()
getEvents = while $ 
  do r <- waitDo ioGetEvents
     if filter (== SDL.Quit) r /= []
     then stop
     else mapM_ emit r >> again

printAll :: Show a => EventStream a -> IO ()
printAll evs = forever $ 
               do v <- cur (decomp (nextSim evs))
                  x <- waitIO v
                  putStrLn (show x)

drawAll :: SDL.Surface -> Behaviour [Box] -> IO ()
drawAll screen b =  forever $ 
   do v <- cur b
      drawBoxes screen v
      wait $ change b


-- nice while thing
again = return True
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
ioGetEvents = do putStrLn "Getting evs!"
                 h <- SDL.waitEvent
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

toRect :: Rect -> SDL.Rect
toRect (normalize -> Rect (lx,uy) (rx,dy)) = SDL.Rect (round lx) (round uy) (round (rx - lx)) (round (dy - uy))

normalize (Rect (lx,uy) (rx,dy)) = Rect (min lx rx, min uy dy) (max lx rx, max uy dy)

data Color   = Color {  r :: Double, g :: Double, b :: Double} deriving (Eq,Show)

red = Color 1 0 0 
green = Color 0 1 0

getColor :: SDL.Surface -> Color -> IO SDL.Pixel
getColor s c = 
     let fmt = SDL.surfaceGetPixelFormat s in
     SDL.mapRGB fmt (con r) (con g) (con b)
  where con d = round ( (d c) * 255.0)

data Box     = Box Rect Color deriving (Eq, Show)

