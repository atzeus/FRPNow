{-# LANGUAGE  GADTs, ExistentialQuantification,TypeOperators, ScopedTypeVariables, Rank2Types #-}
module SyncPar where

import Util.TermM
import Util.ConcFlag
import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import TimeIVar 
import Event
import System.IO.Unsafe
import Data.Either

start  :: IO a -> SyncPar s (Event s a)
start = prim . Start

fork :: Event s (SyncPar s a) -> SyncPar s (Event s a)
fork = prim . Fork

type SyncPar s = TermM (SyncParP s)
data SyncParP s a where
  Start :: IO a -> SyncParP s (Event s a)
  Fork  :: Event s (SyncPar s a) -> SyncParP s (Event s a)

data WaitReq s = WaitReq (Event s ([Req s]))
data IOReq s = forall a. IOReq (TIVar s a) (IO a)
type Req s = Either (IOReq s) (WaitReq s)



runSyncPar :: (forall s .SyncPar s (Event s a)) -> IO a
runSyncPar m = do f <- newFlag
                  withTimeStamper $ \stamper ->
                    do t <- endRound stamper
                       (ev,reqs) <- runTIVarSupply (initSyncPar m)
                       start f stamper ev t reqs
 where 
  start flag stamper ev = loop where
    loop time reqs = 
      let reqs'      = expandReqs (fromStamp time) reqs
          (io,waits) = partitionEithers reqs'
      in do mapM_ (startJob flag stamper) io
            case ev `getAt` fromStamp time of
              Just (_,x)  -> return x
              Nothing     -> 
                do waitForSignal flag
                   t <- endRound stamper
                   loop t (map Right waits)

          


expandReqs :: forall s. Time s -> [Req s] -> [Req s]
expandReqs time = loop where
  loop = concatMap (either (\x -> [Left x]) follow)
  follow (WaitReq e) = 
    case e `getAt` time of
       Just (_,r) -> loop r
       Nothing -> [Right (WaitReq e)]



initSyncPar :: forall a s. SyncPar s a -> TIVarSupply s (a,[Req s])
initSyncPar = loop [] where 
    loop rs e = case viewTermM e of
      Return x -> return (x,rs)
      m :>>= f -> case m of
        Start m -> do k <- makeTIVar
                      let r = Left $ IOReq k m
                      loop (r : rs) (f (fromTIVar k))
        Fork e   -> do e' <- planTIVarSupply (fmap initSyncPar e)
                       let r = Right $ WaitReq (fmap snd e')
                       loop (r:rs) (f (fmap fst e'))


planTIVarSupply :: Event s (TIVarSupply s a) -> TIVarSupply s (Event s a)
planTIVarSupply = return . fmap (unsafePerformIO . runTIVarSupply) 

startJob :: Flag -> TimeStamper s -> IOReq s -> IO ()
startJob flag t (IOReq v m)  = 
  do forkIO $ do a <- m
                 writeTIVar t v a
                 signal flag
     return ()


