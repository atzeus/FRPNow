module ConcFlag where

import Control.Concurrent.MVar

type Flag = MVar ()

{- A concurrent flag signaling if an update should take place,
   designed for one reader and multiple writers
   if the flag is set, the reader has not seen the flag yet,
   and setting the flag is idempotent. -}

newFlag :: IO Flag
newFlag = newEmptyMVar

waitForSignal :: Flag -> IO ()
waitForSignal m = takeMVar m 

signal :: Flag -> IO ()
signal v = do b <- tryPutMVar v (); return ()
