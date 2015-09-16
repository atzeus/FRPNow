-----------------------------------------------------------------------------
-- |
-- Module      :  Control.FRPNow
-- Copyright   :  (c) Atze van der Ploeg 2015
-- License     :  BSD-style
-- Maintainer  :  atzeus@gmail.org
-- Stability   :  provisional
-- Portability :  portable
--
-- An FRP library with first-class and higher-order behaviors, and internalized IO. 
--
-- Based on the paper <http://www.cse.chalmers.se/~atze/papers/prprfrp.pdf Principled Practical FRP: Forget the past, Change the future, FRPNow!>, ICFP 2015, by Atze van der Ploeg and Koenem Claessem.
--
-- The packages @FRPNow-GTK@ and @FRPNow-Gloss@ hook up FRPNow to GUI toolkits via the functions 'Control.FRPNow.GTK.runNowGTK' and 'Control.FRPNow.Gloss.runNowGloss'
--
--
-- To understand what is going on, I suggest you look at the <https://github.com/atzeus/FRPNow/tree/master/Examples examples>, and read section 1-5 of the <http://www.cse.chalmers.se/~atze/papers/prprfrp.pdf paper>. 
--
-- The package contains the following modules:
--
--  [@Core@] The core FRP primitives with denotational semantics.
--  [@Lib@] Utility functions.
--  [@EvStream@] Event streams.
--  [@Time@] Utility functions related to passing the of time.
--  [@BehaviorEnd@] A monadic abstraction for behaviors consisting of multiple phases (a bit advanced stuff, not needed to get going).

module Control.FRPNow( module Control.FRPNow.Core, module Control.FRPNow.Lib, module Control.FRPNow.EvStream, module Control.FRPNow.Time, module Control.FRPNow.BehaviorEnd) where

import Control.FRPNow.Core
import Control.FRPNow.Lib
import Control.FRPNow.EvStream
import Control.FRPNow.Time
import Control.FRPNow.BehaviorEnd
