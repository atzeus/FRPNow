module ConcFlag where

type Flag = MVar ()

{- A concurrent flag signaling if an update should take place,
   designed for one reader and multiple writers
   if the flag is set, the reader has not seen the flag yet,
   and resetting the flag is idempotent. -}


waitForSignal :: Flag -> IO ()
waitForSignal = readMVar

signal :: Flag -> IO Bool
signal v = tryPutMVar v ()