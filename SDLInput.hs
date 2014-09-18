{-# LANGUAGE ViewPatterns, RecursiveDo, ScopedTypeVariables #-}

import IO.Implementation 
import qualified Graphics.UI.SDL as SDL
import Control.Monad.Fix
import Graphics.UI.SDL.Keysym
import Control.Applicative hiding (empty)
import Control.Concurrent
import Control.Monad hiding (when)
import Lib
import EventStream
import Debug.Trace
import Data.Set
import Prelude hiding (until)

type Point     = (Double,Double) -- in pixels
data MouseBtn  = MLeft | MMiddle | MRight deriving (Ord,Eq,Show)

data Rect    = Rect {leftup :: Point, rightdown :: Point} deriving (Eq,Show)

(.+) :: Point -> Point -> Point
(x,y) .+ (x',y') = (x+x', y + y')

(.-) :: Point -> Point -> Point
(x,y) .- (x',y') = (x-x', y - y')

moveRect :: Rect -> Point -> Rect
moveRect (Rect lu rd) p = Rect (lu .+ p) (rd .+ p)

data Color   = Color {  r :: Double, g :: Double, b :: Double} deriving (Eq,Show)
data Box     = Box Rect Color deriving (Eq, Show)

main = do screen <- initSDL
          runNow (mainFRP screen)
          error "To lazy to clean up"

mainFRP :: SDL.Surface -> Now s (Event s ()) 
mainFRP screen = 
          do (evs, end) <- runEventStreamM getEvents
             mousePos <- liftB  $ toMousePos evs
             mButsDown <- liftB $ toMouseButtonsDown evs
             b <- liftB $ boxes mousePos mButsDown evs
             runEventM $ drawAll screen b
             return end

boxes :: Behaviour s Point -> Behaviour s (Set MouseBtn) -> EventStream s SDL.Event -> Behaviour s (Behaviour s [Box])
boxes mousePos mButsDown evs = do let es = filterES isLeftClick evs
                                  let es' = mapES (\(SDL.MouseButtonDown x y _) -> (fromIntegral x, fromIntegral y)) es
                                  let es2 = mapES (box mousePos mButsDown) es'
                                  e <- foldES consBE (pure (pure [])) es2
                                  join e
               

 where isLeftClick (SDL.MouseButtonDown _ _ SDL.ButtonLeft) = True
       isLeftClick _ = False

drawAll :: SDL.Surface -> Behaviour s [Box] -> EventM Now s ()
drawAll screen b =  loop where
  loop = do v <- liftB $ b
            waitIO $ drawBoxes screen v
            wait $ (/= v) <$> b
            loop
                
             
consBE ::Behaviour s (Behaviour s [a]) -> Behaviour s (BehaviourEnd s a ()) ->  Behaviour s (Behaviour s [a]) 
consBE e b = do b' <- b
                e' <- e
                return $ b' .: e'            


initSDL = do  SDL.init [SDL.InitEverything]
              SDL.setVideoMode 800 600 32 [SDL.DoubleBuf]
              SDL.getVideoSurface


isDown :: MouseBtn -> Behaviour s (Set MouseBtn) -> Behaviour s Bool
isDown m b = member m <$> b

click m b = becomesTrue $ isDown m b

release m b = becomesTrue $ not <$> isDown m b



box :: forall s. Behaviour s Point -> Behaviour s (Set MouseBtn) -> Point -> Behaviour s (BehaviourEnd s Box ())
box mouse mouseDown p = runUntilM meat  where
  meat = do Box r _ <- defineBox `untilbl` click MRight mouseDown
            dragBox r
            return ()
  dragBox :: Rect -> UntilM Behaviour Box s ()
  dragBox r = do (mo,dragRect) <- liftB $ 
                   mdo let mo = mouseOver dr
                       shouldMove <- dragging mo
                       dr <- dragRect shouldMove r
                       return (mo,dr)
                 (Box <$> dragRect <*> (toColor <$> mo)) `untilbl` clickInside mo
                 return ()
                        
  toColor True = red
  toColor False = green

  clickInside mouseOver = runEventM loop where
     loop = do waitB $ click MMiddle mouseDown
               r <- liftB $ mouseOver
               if r 
               then return ()
               else loop
                    
  dragging mouseOver = runUntilMl loop where
    loop :: UntilM Behaviour Bool s ()
    loop =
       do pure False `untilb` click MLeft mouseDown
          trace "Jaa" $ return ()
          r <- liftB $ mouseOver
          trace "nee" $ return ()
          if r
          then pure True `untilB` (not <$> isDown MLeft mouseDown)
          else return ()
          loop

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
                
  defineBox = Box <$> rectMouse <*> pure red 
  rectMouse = normalize <$> Rect p <$> mouse



-- boxes :: EventStream s () -> EventStream s () -> Point ->  Behaviour s [Box]
-- boxes mouse rdown lups = fmap (\x -> [x]) $ box mouse rdown lups (100,100) 

{-
quit :: EventStream s SDL.Event -> Behaviour s (Event s ())
quit es = nextES $ fmap (const ()) $ filterES isQuit es where
  isQuit SDL.Quit = True
  isQuit _ = False



toReleases m es = fmap (mapES (const ())) $ filterES isRelease es where
  isRelease (SDL.MouseButtonUp _ _ me) | m == me = True
  isRelease _ = False


-}

toMouseButtonsDown :: EventStream s SDL.Event -> Behaviour s (Behaviour s (Set MouseBtn))
toMouseButtonsDown = foldES updateSet empty where
  updateSet s (SDL.MouseButtonDown _ _ m) | Just m' <- toM m = insert m' s
  updateSet s (SDL.MouseButtonUp   _ _ m) | Just m' <- toM m = delete m' s
  updateSet s _                                              = s

toM SDL.ButtonLeft    = Just MLeft
toM SDL.ButtonMiddle  = Just MMiddle
toM SDL.ButtonRight   = Just MRight
toM _             = Nothing

toMousePos :: EventStream s SDL.Event -> Behaviour s (Behaviour s Point)
toMousePos = foldES getMousePos (0.0,0.0)
  where getMousePos p (SDL.MouseMotion x y _ _) = (fromIntegral x, fromIntegral y)
        getMousePos p _                         = p




getEvents ::  EventStreamM Now SDL.Event s ()
getEvents = loop where
 loop = do r <- waitIO $ ioGetEvents
           mapM_ emit r
           loop


ioGetEvents :: IO [SDL.Event]
ioGetEvents = do h <- SDL.waitEvent
                 t <- loop
                 putStrLn (show (h : t)) 
                 return (h : t)
  where loop = do h <- SDL.pollEvent 
                  case h of
                    SDL.NoEvent -> return []
                    _       -> do t <- loop ;  return (h : t)


getColor :: SDL.Surface -> Color -> IO SDL.Pixel
getColor s c = 
     let fmt = SDL.surfaceGetPixelFormat s in
     SDL.mapRGB fmt (con r) (con g) (con b)
  where con d = round ( (d c) * 255.0)



drawBox :: SDL.Surface -> Box -> IO ()
drawBox s (Box r c) =
  do p <- getColor s c
     --putStrLn (show r)
     SDL.fillRect s (Just $ toRect r) p
     return ()
     
drawBoxes s l = 
  do p <- getColor s (Color 0 0 0)
     SDL.fillRect s (Just $ SDL.Rect 0 0 1200 1000) p
     mapM_ (drawBox s) (reverse l)
     SDL.flip s


isInside :: Point -> Rect -> Bool
isInside (x,y) (normalize -> Rect (l,u) (r,d)) = x >= l && x <= r && y >= u && y <= d

toRect :: Rect -> SDL.Rect
toRect (normalize -> Rect (lx,uy) (rx,dy)) = SDL.Rect (round lx) (round uy) (round (rx - lx)) (round (dy - uy))

normalize (Rect (lx,uy) (rx,dy)) = Rect (min lx rx, min uy dy) (max lx rx, max uy dy)


red = Color 1 0 0 
green = Color 0 1 0
