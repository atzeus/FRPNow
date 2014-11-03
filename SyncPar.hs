{-# LANGUAGE  GADTs, ExistentialQuantification,TypeOperators, ScopedTypeVariables, Rank2Types #-}
module SyncPar where

import Util.TermM
import Util.ConcFlag
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import TimeIVar 
import Event

start  :: IO a -> SyncPar s (Event s a)
start = prim . Start

fork :: Event s (SyncPar s a) -> SyncPar s (Event s a)
fork = prim . Fork

type SyncPar s = TermM (SyncParP s)
data SyncParP s a where
  Start :: IO a -> SyncParP s (Event s a)
  Fork  :: Event s (SyncPar s a) -> SyncParP s (Event s a)

data WaitReq s = forall a. WaitReq (TIVar s a) (Event s (SyncPar s a))
data IOReq s = forall a. IOReq (TIVar s a) (IO a)
type Req s = Either (IOReq s) (WaitReq s)


runSyncPar :: (forall s .SyncPar s (Event s a)) -> IO a
runSyncPar m = do f <- newFlag; withTimeStamper (start f) where
  start f stamper = do t <- endRound stamper
                       (ev,l) <- initSyncPar t
                       let (io,waits) = partitionEithers ev
                       mapM_ (startJob flag stamper) ios 
                       checkEv (info t) t ev (loop flag stamper waits) 

  checkEv time ev cont = 
     case ev `getAt` time of
         Just (_,x)  -> return x
         Nothing     -> cont ev'

  loop flag stamper waits ev  = 
     do waitForSignal flag
        mapM (updateWait (info t) t) waits 
            t <- endRound stamper
        let (io,waits) = partitionEithers ev
        mapM_ (startJob flag stamper) ios 
        checkEv (info t) t ev (loop flag stamper waits) 


  updateWait (WaitReq v e) time = 
       case e `getAt` time of
        Just (t,x) ->  


initSyncPar :: forall a s. Time s -> SyncPar s a -> TIVarSupply s (a,[Req s])
initSyncPar time = loop [] where 
	loop :: forall a . [Req s] -> SyncPar s a -> TIVarSupply s (a,[Req s])
	loop l e = case viewTermM e of
	  Return x -> return (x,l)
	  m :>>= f -> case m of
		Start m -> do k <- newTIVar
		              let w = Left $ IOReq k m
		              loop (w : l) (f (fromTIVar k))
		Fork e -> case e `getAt` time of
		               Just (t,x) -> do (v, l') <- loop l x 
		                                let ev = makeEvent t v
		                                loop l' (f ev)
		               Nothing -> do k <- newTIVar
		                             let w = Right $ WaitReq k e
		                             loop (w:l) (f (fromTIVar k))


startJob :: Flag -> TimeStamper s -> TIVar s a -> IO a -> IO ()
startJob flag t v m  = 
  do forkIO $ do a <- m
                 writeTIVar t v a
                 signal flag
     return ()


