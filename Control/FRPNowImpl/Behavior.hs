{-# LANGUAGE RecursiveDo, ScopedTypeVariables, LambdaCase #-}
module Control.FRPNowImpl.Behavior(Behavior,curIO, switch, whenJust, seqb, seqAlways) where
import Control.Applicative hiding (empty,Const)
import Control.Monad
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.FRPNowImpl.Now


