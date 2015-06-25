
module Examples.SimpleGraphics where

import Data.Set

type Point = (Double,Double)
type Vec   = Point

(.+) :: Point -> Point -> Point
(xa,ya) .+ (xb,yb) = (xa + xb, ya + yb)

(.-) :: Point -> Point -> Point
(xa,ya) .- (xb,yb) = (xa - xb, ya - yb)

(.*) ::  Double -> Point -> Point
s .* (x,y) = (s * x, s * y)


data Rect  = Rect { leftup :: Point, rightdown :: Point } deriving (Eq,Show)

sort2 :: Double -> Double -> (Double,Double)
sort2 x y | x < y     = (x,y)
          | otherwise = (y,x)

rect :: Point -> Point -> Rect
rect (xa,ya) (xb,yb) = Rect (xmin,ymin) (xmax,ymax)
            where (xmin,xmax) = sort2 xa xb
                  (ymin,ymax) = sort2 ya yb



moveRect :: Rect -> Vec -> Rect
moveRect (Rect lu rd) p = Rect (lu .+ p) (rd .+ p)

isInside :: Point -> Rect -> Bool
isInside (x,y) (Rect (l,u) (r,d)) = x >= l && x <= r && y >= u && y <= d

data Color = C { redC :: !Double, greenC :: !Double, blueC :: Double } deriving (Eq,Show, Ord)

lerp :: Double -> Double -> Double -> Double
lerp l f r | f >= 1 = r
           | f <= 0 = l
           | otherwise = l * (1 - f) + r * f


lerpPoint :: Double -> Point -> Point -> Point
lerpPoint f l@(lx,ly) r@(rx,ry)
       | f >= 1 = r
       | f <= 0 = l
       | otherwise = (lx * (1 - f) + rx * f, ly * (1 - f) + ry * f)

lerpColor :: Color -> Double -> Color -> Color
lerpColor (C ra ga ba) f (C rb gb bb) = C (lerp ra f rb) (lerp ga f gb) (lerp ba f bb)

data Box = Box { color :: Color, rectBox :: Rect } deriving (Eq,Show)

moveBox :: Box -> Vec -> Box
moveBox (Box c r) v = Box c (moveRect r v)

data MouseBtn = MLeft | MMiddle | MRight deriving (Ord,Eq, Show)

type BtnAction = (MouseBtn, Bool)

updateSet :: Set MouseBtn -> BtnAction -> Set MouseBtn
updateSet s (b,i) = action i b s
    where action False = delete
          action True  = insert

red = C 1.0 0 0 
green = C 0 1.0 0.0
blue = C 0 0.0 1.0
black = C 0 0 0
white = C 1 1 1
grey  = C 0.5 0.5 0.5

