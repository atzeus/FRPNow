{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}


import Control.Monad.Fix
import Control.Applicative hiding (empty)
import Control.Monad hiding (when)
import Control.FRPNow
import Control.FRPNow.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss
import Debug.Trace
import Data.Maybe
import Data.Set hiding (filter,fold, foldl,map)
import Prelude hiding (until)

main = runNowGloss (InWindow "FRPNow Gloss!" (800,600) (10,10)) white 60 mainFRP

lerpColor :: Float -> Color -> Color -> Color
lerpColor d ca cb = mixColors d (1-d) ca cb

mainFRP :: Behavior Float -> EvStream GEvent -> Now (Behavior Picture)
mainFRP time evs = 
   let posSin x = (sin x + 1) / 2.0
       pict b time (x,y) = Color (if b then red else green) $ 
                           Translate x y $
                           ThickCircle 25 ((posSin time) * 50 + 10)
       done = Translate (-300) 0 $ Scale 0.4 0.4 $ Text "I'm done with it"
   in do mousePos <- sample (toMousePos evs)
         keys     <- sample (toKeysDown evs)
         let spaceDown = (SpecialKey KeySpace `member`) <$> keys

         e <- sample $ when ((>= 30.0) <$> time)
         plan (sync (putStrLn "You've done this for half a minute!") <$ e)
         
         let init = pict <$> spaceDown <*> time <*> mousePos
         let b    = init `switch` (pure done <$ e)
         return b

