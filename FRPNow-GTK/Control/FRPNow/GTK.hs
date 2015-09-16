-----------------------------------------------------------------------------
-- |
-- Module      :  Control.FRPNow.GTK
-- Copyright   :  (c) Atze van der Ploeg 2015
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
-- 
-- This module provides interoperability of FRPNow and the GTK system.

module Control.FRPNow.GTK(
  -- * General interface
  runNowGTK, setAttr, getSignal, getUnitSignal, getSimpleSignal, getClock,
  -- * Utility functions
  createLabel, createButton, createProgressBar,createSlider
  ) where

import Graphics.UI.Gtk
import Control.Applicative
import Control.FRPNow
import Data.Maybe
import Data.IORef
import Debug.Trace
import System.Mem.Weak
import System.Glib.GDateTime

-- | Run a Now computation which can interact with GTK. Also starts the GTK system.
-- Call only once, or GTK will freak out.
runNowGTK :: Now () -> IO ()
runNowGTK n = do initGUI
                 doneRef <- newIORef Nothing
                 initNow (schedule doneRef) (n >> return never)
                 mainGUI
                     
                 
                 
schedule :: IORef (Maybe a) -> IO (Maybe a) -> IO () 
schedule ref m = postGUIAsync $ 
                   m >>= \x ->
                     case x of
                      Just _ -> writeIORef ref x 
                      Nothing -> return ()
                       
-- | Set a GTK attribute to a behavior. Each time the behavior changes the
-- attribute is updated.
setAttr :: (WidgetClass w, Eq a) => Attr w a -> w -> Behavior a -> Now ()
setAttr a w b = 
     do i <- sample b
        sync $ set w [a := i]
        (e,cb) <- callback 
        sync $ on w unrealize ( cb ())
        let updates = toChanges b `beforeEs` e
        callIOStream setEm updates
  where setEm i = set w [a := i] >> widgetQueueDraw w


-- | Obtain an event stream from a unit GTK signal, i.e. a signal with handler type:
-- 
-- > IO ()
getUnitSignal :: GObjectClass widget => Signal widget (IO ()) -> widget -> Now (EvStream ())
getUnitSignal s w = getSignal s w (\f -> f ())
  

-- | Obtain an event stream from a GTK signal giving a single value.
getSimpleSignal :: GObjectClass widget => Signal widget (value -> IO ()) -> widget -> Now (EvStream value)
getSimpleSignal s w = getSignal s w id 


-- | General interface to convert an GTK signal to an event stream. 
--
-- The signal has type @callback@, for example @(ScrollType -> Double -> IO Bool)@ 
-- and the eventstream gives elements of type @value@, for instance @(ScrollType,Double)@
-- The conversion function (3rd argument) takes a function to call for producing the value 
-- in our example, a function of type @(ScollType,Double) -> IO ()@ and produces
-- a function of the form @callback@, in our example @(ScrollType -> Double -> IO Bool)@.
--
-- In this example we can convert a signal with handler @(ScrollType -> Double -> IO Bool)@
-- to an eventstream giving elements of type @(ScrollType,Double)@ by letting the handler return @False@
-- as follows:
--
-- > scrollToEvStream :: Signal widget (ScrollType -> Double -> IO Bool) -> widget -> Now (EvStream (ScrollType,Double))
-- > scrollToEvStream s w = getSignal s w convert where
-- >   convert call scrolltype double = do call (scrolltype, double)
-- >                                       return False
--
-- The signal is automatically disconnected, when the event stream is garbage collected.
getSignal :: GObjectClass widget => Signal widget callback -> widget -> ((value -> IO ()) -> callback) -> Now (EvStream value)
getSignal s w conv =    
   do (res,f) <- callbackStream
      conn <- sync $ on w s (conv f)
      --sync $ addFinalizer res (putStrLn "Run final" >> signalDisconnect conn)
      return res


-- | Get a clock that gives the time since the creation of the clock in seconds, and updates maximally even given number of seconds.
--
-- The clock is automatically destroyed and all resources associated with the clock are freed 
-- when the behavior is garbage collected.
getClock :: Double -> Now (Behavior Double)
getClock precision = 
  do start <- sync $ gGetCurrentTime
     (res,cb) <- callbackStream
     wres<- sync $ mkWeakPtr res Nothing
     let getDiff = do now <- gGetCurrentTime
                      let seconds = gTimeValSec now - gTimeValSec start
                      let microsec = gTimeValUSec now - gTimeValUSec start
                      return $ (fromIntegral seconds) + (fromIntegral microsec) * 0.000001
     let onTimeOut = 
              deRefWeak wres >>= \x -> 
                 case x of
                   Just _ -> getDiff >>= cb >> return True
                   Nothing -> return False
     sync $ timeoutAdd  onTimeOut (round (precision * 1000)) 
     sample $ fromChanges 0 res



createLabel :: Behavior String -> Now Label
createLabel s = 
  do l <- sync $ labelNew (Nothing :: Maybe String)
     setAttr labelLabel l s
     return l


createButton :: Behavior String ->  Now (Button,EvStream ())
createButton s =  
  do button <- sync $ buttonNew 
     setAttr buttonLabel button s
     stream <- getUnitSignal buttonActivated  button
     return (button,stream)


createProgressBar :: Now (ProgressBar, Double -> IO ()) 
createProgressBar =
              do (evs, cb) <- callbackStream
                 progress <- sample $ fromChanges 0 evs
                 bar <- sync $ progressBarNew
                 setAttr progressBarFraction bar progress
                 return (bar,cb)

createSlider ::  Double -> Double -> Double -> Behavior Double -> Now (HScale,EvStream Double)
createSlider min max step b =  
  do i <- sample b
     slider <- sync $ hScaleNewWithRange min max step
     setAttr rangeValue slider b
     stream <- getSignal changeValue slider (\f _ d -> f d >> return True) 
     return (slider,stream)



