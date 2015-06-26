-----------------------------------------------------------------------------
-- |
-- Module      :  Control.FRPNow.Gloss
-- Copyright   :  (c) Atze van der Ploeg 2015
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
-- 
-- This module provides interoperability of FRPNow and the gloss system.

module Control.FRPNow.Gloss(GEvent,runNowGloss,toMousePos, toKeysDown, filterMouseButtons) where

import Graphics.Gloss.Interface.IO.Game hiding (Event)
import Control.FRPNow
import Data.Sequence
import qualified Data.Sequence as Seq
import Data.Maybe
import Data.IORef
import Debug.Trace
import GHC.Float
import qualified Data.Foldable as Fold
import Data.Set
import qualified Data.Set as Set
import qualified Graphics.Gloss.Interface.IO.Game as Gloss
import Debug.Trace

-- | Alias for 'Gloss.Event' to prevent name clash with 'Event'.
type GEvent = Gloss.Event

runNowGloss :: 
            Display  -- ^ Display mode.
         -> Color    -- ^ Background color.
         -> Int      -- ^ Maximum number of frames per second 
         -> (Behavior Float -> EvStream GEvent -> Now (Behavior Picture))  -- ^ A now computation giving the picture to be displayed on the screen, taking the behavior of time and the eventstream of gloss events.
         -> IO ()
runNowGloss disp bg fps m = 
  do scheduleRef <- newIORef Seq.empty
     callbackRef <- newIORef undefined
     pictureRef <- newIORef Blank
     initNow (schedule scheduleRef) (initM callbackRef pictureRef)
     (cbTime, cbgEv) <- readIORef callbackRef
     playIO disp bg fps ()
         (\_ -> readIORef pictureRef)
         (\ev _ -> cbgEv ev)
         (\deltaTime _ -> do cbTime deltaTime
                             rounds <- readIORef scheduleRef
                             writeIORef scheduleRef Seq.empty
                             mapM_ id (Fold.toList rounds)
                             return ()
         )
          
                 
    where 
  initM callbackRef pictureRef = 
     do (timeEvs,cbtime) <- callbackStream
        (gevEvs,cbgEv)   <- callbackStream
        sync $ writeIORef callbackRef (cbtime,cbgEv)
        clock <- sample $ foldEs (+) 0 timeEvs
        pict <- m clock gevEvs
        curPict <- sample pict
        sync $ writeIORef pictureRef curPict
        callIOStream (writeIORef pictureRef) (toChanges pict)
        return never

  schedule ref m = atomicModifyIORef ref (\s -> (s |> m, ())) 

toMousePos :: EvStream GEvent -> Behavior (Behavior (Float, Float))
toMousePos evs = foldEs updateMouse (0,0) evs where
  updateMouse _ (EventMotion p)  = p
  updateMouse p _                = p

toKeysDown :: EvStream GEvent -> Behavior (Behavior (Set Key))
toKeysDown evs = foldEs updateSet Set.empty evs where
  updateSet :: Set Key -> GEvent -> Set Key
  updateSet s (EventKey k i _ _) = action i k s
      where action Up    = delete
            action Down  = insert
  updateSet s _ = s

filterMouseButtons :: Behavior (Set Key) -> Behavior (Set MouseButton)
filterMouseButtons b = 
     let isMouseButton (MouseButton _) = True
         isMouseButton _               = False
     in Set.map (\(MouseButton x) -> x) . Set.filter isMouseButton <$> b
