{-# LANGUAGE ViewPatterns #-}

import IO.Implementation 
import qualified Graphics.UI.SDL as SDL
import qualified Data.Set as Set
import Graphics.UI.SDL.Keysym
import Control.Applicative
import Control.Concurrent
import Control.Monad hiding (when)
import Lib
import Debug.Trace

type Point     = (Double,Double) -- in pixels
data MouseBtn  = MLeft | MMiddle | MRight deriving (Ord,Eq,Show)

data Rect    = Rect {leftup :: Point, rightdown :: Point} deriving (Eq,Show)
data Color   = Color {  r :: Double, g :: Double, b :: Double} deriving (Eq,Show)
data Box     = Box Rect Color deriving (Eq, Show)

main = do screen <- initSDL
          runNow (mainFRP screen)
          error "To lazy to clean up"

mainFRP :: SDL.Surface -> Now s (Event s ()) 
mainFRP screen = --return () >> return never
          do ev <- getEvents
             mousePos <- liftB $ toMousePos ev
             leftClicks <- liftB $ toClicks SDL.ButtonLeft ev
             rightUps    <- liftB $toReleases SDL.ButtonRight ev
             rup <- liftB $ nextES rightUps
             (b,e) <- liftB $ runUntilM $ box mousePos leftClicks rup (100,100)
             --q <- liftB $ quit ev
             drawAll screen b

             test ev mousePos
             return never


test es b = loop
  where loop   = do ev <- liftB $ nextES es
                    v <- liftB $ b
                    printState b 
                    -- e <- liftB $ when (/= v) b
                    planNow (fmap (const loop) ev)
                    return ()

drawAll :: SDL.Surface -> Behaviour s Box -> Now s ()
drawAll screen b =  loop where
  loop = do v <- liftB $ b
            act (drawBoxes screen [v])
            e <- liftB $ when (/= v) b
            planNow (fmap (const loop) e)
            return ()
                
             
             


initSDL = do  SDL.init [SDL.InitEverything]
              SDL.setVideoMode 800 600 32 [SDL.DoubleBuf]
              SDL.getVideoSurface




box :: Behaviour s Point -> EventStream s () -> Event s () -> Point ->  UntilM Behaviour s Box ()
box mouse ldown rup p =
  do defineBox `untill` rup
     Box r _ <- self
     pure (Box r red) `untill` never
     return ()
  where 
        defineBox = Box <$> rectMouse <*> pure red 
        rectMouse = normalize <$> Rect p <$> mouse
        red = Color 1 0 0 
-- boxes :: EventStream s () -> EventStream s () -> Point ->  Behaviour s [Box]
-- boxes mouse rdown lups = fmap (\x -> [x]) $ box mouse rdown lups (100,100) 

{-
quit :: EventStream s SDL.Event -> Behaviour s (Event s ())
quit es = nextES $ fmap (const ()) $ filterES isQuit es where
  isQuit SDL.Quit = True
  isQuit _ = False
-}
toClicks :: SDL.MouseButton -> EventStream s SDL.Event -> StartEventStream s ()
toClicks m es = fmap (mapES (const ())) $ filterES isClick es where
  isClick (SDL.MouseButtonDown _ _ me) | m == me = True
  isClick _ = False

toReleases m es = fmap (mapES (const ())) $ filterES isRelease es where
  isRelease (SDL.MouseButtonUp _ _ me) | m == me = True
  isRelease _ = False



toMousePos :: EventStream s SDL.Event -> StartBehaviour s Point
toMousePos = foldES getMousePos (0,0)
  where getMousePos p (SDL.MouseMotion x y _ _) = (fromIntegral x, fromIntegral y)
        getMousePos p _                         = p



getEvents ::  Now s (EventStream s SDL.Event)
getEvents = loop where
 loop = do e <- act ioGetEvents
           n <- planNow (fmap (const loop) e)
           return (pure e `switch` n)


ioGetEvents :: IO [SDL.Event]
ioGetEvents = do h <- SDL.waitEvent
                 t <- loop
                 -- putStrLn (show (h : t)) 
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
     SDL.fillRect s (Just $ toRect r) p
     return ()
     
drawBoxes s l = 
  do putStrLn "Drawing!"
     p <- getColor s (Color 0 0 0)
     SDL.fillRect s (Just $ SDL.Rect 0 0 1200 1000) p
     mapM_ (drawBox s) (reverse l)
     SDL.flip s


toRect :: Rect -> SDL.Rect
toRect (normalize -> Rect (lx,uy) (rx,dy)) = SDL.Rect (round lx) (round uy) (round (rx - lx)) (round (dy - uy))

normalize (Rect (lx,uy) (rx,dy)) = Rect (min lx rx, min uy dy) (max lx rx, max uy dy)
