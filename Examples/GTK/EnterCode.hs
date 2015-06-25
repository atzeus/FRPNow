{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}

import Graphics.UI.Gtk
import Control.FRPNow
import Control.FRPNow.GTK
import Control.FRPNow.Time
import Control.Applicative
import Control.Concurrent
import Control.Monad.Trans
import Data.List
main :: IO ()
main = runNowGTK $ mdo

  -- initialization code
  window <- sync $ windowNew
  sync $  set window [ containerBorderWidth := 10 ]
  vbox <- sync $  vBoxNew False 20
  hbox <- sync $  hBoxNew False 20
  sync $  set window [ containerChild := vbox ]
  sync $ window `on` deleteEvent $ liftIO mainQuit >> return False



  (ba,aclicks) <- createButton (pure "a")
  (bb,bclicks) <- createButton (pure "b")
  clock <- getClock 0.03

  -- logic 
  let abclicks = ('a' <$ aclicks) `merge` ('b' <$ bclicks)
  -- get letters in last 4 seconds
  input <- sample $ lastInputs clock 4 abclicks
  -- get the event the code is correct
  correctEv <- sample $ when $ ("abbab" `isSuffixOf`) <$> input

  -- display current buffer
  let buffer = ("Current buffer: " ++) <$> input
  let text = buffer `switch` (pure "correct!" <$ correctEv)

  label <- createLabel text

  -- layout and more initialization
  expl <- sync $ labelNew (Just "Type abbab within 4 seconds!")
  sync $  boxPackStart vbox expl  PackNatural 0
  sync $  boxPackStart hbox ba PackNatural 0
  sync $  boxPackStart hbox bb PackNatural 0
  sync $  boxPackStart vbox hbox  PackNatural 0
  sync $  boxPackStart vbox label PackNatural 0
  sync $  widgetShowAll window

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
