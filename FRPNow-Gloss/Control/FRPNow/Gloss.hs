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

module Control.FRPNow.Gloss(GEvent,Time,runNowGloss, runNowGlossPure, toMouseMoves, toMousePos, toKeysDown, filterMouseButtons) where

import Graphics.Gloss.Interface.IO.Game hiding (Event)
import Control.FRPNow
import Data.Sequence
import Control.Applicative
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
-- | The gloss type for time.
type Time = Float 

-- | Run a Now computation which produced a behavior of type Picture, and draw that on screen.
runNowGloss :: 
            Display  -- ^ Display mode.
         -> Color    -- ^ Background color.
         -> Int      -- ^ Maximum number of frames per second 
         -> (Behavior Time -> EvStream GEvent -> Now (Behavior Picture))  -- ^ A now computation giving the picture to be displayed on the screen, taking the behavior of time and the eventstream of gloss events.
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

-- | Like 'runNowGloss', but does not allow IO.
runNowGlossPure ::   
            Display  -- ^ Display mode.
         -> Color    -- ^ Background color.
         -> Int      -- ^ Maximum number of frames per second 
         -> (Behavior Time -> EvStream GEvent -> Behavior (Behavior Picture))  -- ^ A behavior giving the picture to be displayed on the screen, taking the behavior of time and the eventstream of gloss events.
         -> IO ()
runNowGlossPure disp bg fps b = runNowGloss disp bg fps (\t e -> sample $ b t e)

-- | Filter the mouse moves from an event stream of gloss events
toMouseMoves :: EvStream GEvent -> EvStream (Float,Float) 
toMouseMoves evs = filterMapEs getMouseMove evs
  where getMouseMove (EventMotion p) = Just p
        getMouseMove _               = Nothing

-- | Get a behavior of the mouse position from an event stream of gloss events
toMousePos :: EvStream GEvent -> Behavior (Behavior (Float, Float))
toMousePos evs = fromChanges (0,0) (toMouseMoves evs) 

-- | Get a behavior of the set of currently pressed keys from an event stream of gloss events
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
