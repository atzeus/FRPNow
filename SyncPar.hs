{-# LANGUAGE  GADTs, ExistentialQuantification,TypeOperators, ScopedTypeVariables, Rank2Types #-}
module SyncPar where

import Util.TermM
import Util.ConcFlag
import qualified Event as E
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Util.ConcList
import TimeIVar 

data EWait s a where
  IOWait   :: TIVar s a  -> EWait s a
  ForkWait :: TIVar s a  -> EWait s a 

type Event s = E.Event s (EWait s) 

start    :: IO a -> SyncPar s (Event s a)
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
                       (ev,l) <- runTIVarSupply $ initSyncPar (info t) t
                       let (io,waits) = partitionEithers ev
                       mapM_ (startJob flag stamper) ios 
                       checkEv (info t) t ev (loop flag stamper waits) 

  checkEv info time ev cont = 
     let ev' = E.updateEvent info time e
     in case E.getEvent e' of
         Just x  -> return x
         Nothing -> cont ev'

  loop flag stamper waits ev  = 
     do waitForSignal flag
        mdo l <- mapM (updateWait (info t) t) waits 
            t <- endRound stamper
        let (io,waits) = partitionEithers ev
        mapM_ (startJob flag stamper) ios 
        checkEv (info t) t ev (loop flag stamper waits) 

  info t (IOWait   tv) = readTIVarAt tv t
  info t (ForkWait tv) = readTIVarAt tv t


type Info s = forall a. EWait s a -> Maybe a


initSyncPar :: forall s a .Info s -> E.Time s ->  SyncPar s a -> TIVarSupply s (a,[Req s])
initSyncPar info time = loop [] where 
	loop :: forall a. [Req s] -> SyncPar s a -> TIVarSupply s (a,[Req s])
	loop l e = case viewTermM e of
	  Return x -> return (x,l)
	  m :>>= f -> case m of
		Start m -> do k <- makeTIVar
		              let w = Left $ IOReq k m
		              loop (w : l) (f (E.waitOn (IOWait k)))
		Fork e -> let e' = E.updateEvent info time e
		          in case E.getEvent e' of
		               Just x -> do (v, l') <- loop l x 
		                            let ev = E.makeAtTime time v
		                            loop l' (f ev)
		               Nothing -> do k <- makeTIVar
		                             let w = Right $ WaitReq k e
		                             loop (w:l) (f (E.waitOn (ForkWait k)))


startJob :: Flag -> TimeStamper s -> TIVar s a -> IO a -> IO ()
startJob flag t v m  = 
  do forkIO $ do a <- m
                 writeTIVar t v a
                 signal flag
     return ()


