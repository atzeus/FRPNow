{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}

import Graphics.UI.Gtk
import Control.FRPNow
import Control.FRPNow.GTK
import Control.Applicative
import Control.Concurrent
import Control.Monad.Trans

main :: IO ()
main = runNowGTK $ mdo

  -- initialization code
  window <- sync $ windowNew
  sync $  set window [ containerBorderWidth := 10 ]
  hbuttonbox <- sync $  hButtonBoxNew
  sync $  set window [ containerChild := hbuttonbox ]
  sync $ window `on` deleteEvent $ liftIO mainQuit >> return False


  -- logic with recursive do
  d <- sample $ fromChanges 0 (e1 `merge` fmap (1 - ) e2) 
  (slider1,e1) <- createSlider 0 1 0.1 d
  (slider2,e2) <- createSlider 0 1 0.1 ((1 -) <$> d)


  -- layout and more initialization
  sync $  set hbuttonbox [ containerChild := button
                 | button <- [slider1, slider2] ]  
  sync $ set hbuttonbox [ buttonBoxLayoutStyle := ButtonboxStart ]
  sync $  widgetShowAll window




