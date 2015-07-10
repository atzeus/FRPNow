{-# LANGUAGE TypeOperators, ViewPatterns, RecursiveDo, ScopedTypeVariables #-}

import Graphics.UI.Gtk
import Control.FRPNow
import Control.FRPNow.GTK
import Control.Applicative
import Control.Concurrent
import Control.Monad.Trans

-- Shows simple usage of FRPNow with GTK

main :: IO ()
main = runNowGTK $ do

  -- initialization code
  window <- sync $ windowNew
  sync $  set window [ containerBorderWidth := 10 ]
  hbuttonbox <- sync $  hButtonBoxNew
  sync $  set window [ containerChild := hbuttonbox ]
  sync $ window `on` deleteEvent $ liftIO mainQuit >> return False


  -- create buttons
  button1 <- clickMeButton
  button2 <- clickMeButton
  button3 <- clickMeButton
  


  -- layout and more initialization
  sync $  set hbuttonbox [ containerChild := button
                 | button <- [button1, button2, button3] ]  
  sync $ set hbuttonbox [ buttonBoxLayoutStyle := ButtonboxStart ]
  sync $  widgetShowAll window



clickMeButton :: Now Button
clickMeButton = mdo count <- sample $ foldEs (\c _ -> c + 1) 0 clicks 
                    enoughEv <- sample $ when ((> 10) <$> count)
                    let clickMessage i = "You've clicked : " ++ show i ++ " times, click more!"
                    let message = (clickMessage <$> count) `switch` (pure "You've clicked enough!" <$ enoughEv)
                    (button, clicks) <- createButton message
                    return button



