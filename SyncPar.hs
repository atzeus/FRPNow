{-# LANGUAGE  GADTs, ExistentialQuantification,TypeOperators, ScopedTypeVariables, Rank2Types #-}
module SyncPar where

import Util.TermM
import qualified Event as E
import Control.Monad
import Control.Monad.Trans
import Control.Concurrent
import Util.ConcList
import Data.HMap
import Data.HKey
import System.IO.Unsafe

data EWait s a where
  IOWait   :: HKey s a                  -> EWait s a
  ForkWait :: HKey s (Maybe a) -> EWait s a -- lazy nastiness

type Event s = E.Event (EWait s) 

start    :: IO a -> SyncPar s (Event s a)
start = prim . Start

fork :: Event s (SyncPar s a) -> SyncPar s (Event s a)
fork = prim . Fork

type SyncPar s = TermM (SyncParP s)
data SyncParP s a where
  Start :: IO a -> SyncParP s (Event s a)
  Fork  :: Event s (SyncPar s a) -> SyncParP s (Event s a)



newtype ContinueEv s a = CE (Event s (SyncPar s a))

data IOReq s =  forall a. IOReq (HKey s a) (IO a)

data Wait s = forall a. Wait (HKey s (Maybe a)) (Event s (SyncPar s a))

type Req s = Either (IOReq s) (Wait s)

data Res  s = forall a. Res (HKey s a) (Maybe a)

{-
runSyncPar :: (forall s .SyncPar s (Event s a)) -> IO a
runSyncPar m = do cl <- newConcList
                  runKeyT $ initSyncPar cl >>= start cl where

start :: forall s a. ConcList (Res s) -> (Event s a, [Wait s]) -> KeyT s IO a
start cl (e,l) = loop e l empty where
  loop e l m t = 
     do m' <- lift $ purge m
        mdo let l' = map (tryUpdateWait info t) l
            
        let info x = \x -> lookup x m'
        let e' = update info t e
        case getEvent e' of
          Just x -> return x
          Nothing -> do 
   where tryUpdateWait info t (Wait k e) = Wait k (update info t e)
 -}         

round info time =  where

  updateAll 
  
  updateEvent = E.updateEvent info time
{-
  updateWait :: HKey s (Maybe a) -> Event s (SyncPar s a) -> KeyM s (Maybe a,[Req s])
  updateWait e =  
   let e' = updateEvent e
   in case E.getEvent e' of
       Just x -> do (v,l) <- initSyncPar x ; return (Just v,l) 
       Nothing -> return (Nothing, [Right $ Wait k e])
-}
  initSyncPar :: SyncPar s a -> KeyM s (a,[Req s])
  initSyncPar = loop [] where 
    loop :: forall a. [Req s] -> SyncPar s a -> KeyM s (a,[Req s])
    loop l e = case viewTermM e of
      Return x -> return (x,l)
      m :>>= f -> case m of
        Start m -> do k <- getKey
                      let w = Left $ IOReq k m
                      loop (w : l) (f (E.waitOn (IOWait k)))
        Fork e -> let e' = updateEvent e
                  in case E.getEvent e' of
                       Just x -> do (v, l') <- loop l x 
                                    let ev = E.makeAtTime time v
                                    loop l' (f ev)
                       Nothing -> do k <- getKey
                                     let w = Right $ Wait k e
                                     loop (w:l) (f (E.waitOn (ForkWait k)))

{-
  startJob :: HKey s x -> IO x -> IO ThreadId
  startJob k m  = forkIO $ 
      do v <- m
         addConcList (Res k v) cl
-}

