{-# LANGUAGE TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}

module Control.FRPNowImpl.Behaviour(Behaviour,switch,beforeSwitch, whenJust,seqB,curIO) where

import Data.Sequence
import Control.Applicative hiding (empty,Const)
import Control.FRPNowImpl.Event
import Data.Foldable
import Control.Monad
import Control.Monad.Fix
import System.IO.Unsafe
import Control.Concurrent.MVar
import Debug.Trace
import Prelude hiding (null,length)

-- start non-shared behaviour
{-
data Behaviour a where
 Const      :: a -> Behaviour a
 Delay      :: Time -> Behaviour a -> Behaviour a
 Switch     :: Behaviour a -> Event (Behaviour a) -> Behaviour a
 Bnd        :: Behaviour a -> (a -> Behaviour b) -> Behaviour b
 WhenJust   :: Behaviour (Maybe a) -> Behaviour (Event a)
 SeqB       :: Behaviour x -> Behaviour a -> Behaviour a

instance Monad Behaviour where
  return = Const
  (>>=)  = Bnd

switch = Switch
whenJust = WhenJust
seqB = SeqB
delay = Delay
-}
data BehaviourNF a = BNF { lastChange :: Time, val :: a, prev :: a , same :: Bool , switches ::  Seq (Event () ) }

curIO :: Behaviour a -> Now a
curIO m = do nf <- updateGetNF m
             --i <- getShape m
             --trace (show i) $ 
             return (val nf)

getConstant :: Behaviour a -> Now (Maybe (Time,a))
getConstant e = updateGetNF e >$< \nf -> if null (switches nf) then Just (lastChange nf,val nf) else Nothing

{-
getBehaviour :: Behaviour a -> Now (Behaviour a,BehaviourNF a)
getBehaviour e = do e' <- rewriteBehaviour e
                    nf <- getNF e' e'
                    return (e',nf)
-}

{-
getShape :: Behaviour a -> Now String
getShape (B m) = 
         do (_, s, _) <- syncIO $ readMVar m
            case s of
               Const _ -> return ""
               Delay _ t -> ('D' :) <$> getShape t
               Bnd m f   -> ('B' :) <$> getShape m 
               Switch m e -> ('S' :) <$> getShape m
               WhenJust b -> getShape b
-}
getBehaviour :: Behaviour a -> Now (BehaviourSyntax a,BehaviourNF a)
getBehaviour (B m) = 
 do (i, s, mnf) <- syncIO $ takeMVar m
    j  <- trace "get" $ getRound
    (s',nf) <- maybeUpdate i s mnf j
    let nf' = if lastChange nf /= j
              then nf {prev = val nf, same = True }
              else nf
    syncIO $ putMVar m (j,s', Just nf')
    return (s',nf') where

  maybeUpdate i s mnf j = case mnf of
    Just nf 
      | i == j -> return (s,nf)
      | otherwise -> checkAll (toList $ switches nf) >>= \case 
           Just (_,a) -> update mnf s
           Nothing    -> return (s,nf)
    Nothing -> update mnf s

  update mnf s = do s' <- rewriteBehaviour s
                    nf' <- getNF mnf (B m) s'
                    return (s',nf')

-- non-shared : updateRewrite :: Behaviour a -> Now (Behaviour a)
updateRewrite :: Behaviour a -> Now (BehaviourSyntax a)
updateRewrite e = fst <$> getBehaviour e


updateGetNF :: Behaviour a -> Now (BehaviourNF a)
updateGetNF e = snd <$> getBehaviour e



-- getNF :: Behaviour a -> Behaviour a -> Now (BehaviourNF a)
getNF :: Maybe (BehaviourNF a) -> Behaviour a -> BehaviourSyntax a -> Now (BehaviourNF a)
getNF mnf self e = case e of
  Const x    -> return $ BNF minBound x x True empty

  Delay t b  -> do nf <- updateGetNF b
                   return nf {lastChange = max t (lastChange nf)}

  BeforeSwitch b -> do nfb <- updateGetNF b
                       return nfb {val = prev nfb, same = True}

  MFix f -> mfix (\nfb -> trace "Jada" $ updateGetNF (f (val nfb)))

  -- we rewrote just before this, means just switched.
  Switched i b v -> do nfb <- updateGetNF b
                       nfv <- updateGetNF v
                       return $ BNF i (val nfv) (val nfb) False (switches nfv)

  Switch b e -> do nf <- updateGetNF b
                   let e' = () <$ e
                   return nf {switches = e' <| switches nf }

  Bnd b m f  -> do nfb <- updateGetNF b
                   mv <- syncIO $ takeMVar m
                   let (i,v) = case mv of
                               Just (i,v) | i == lastChange nfb  -> (i,v)
                               _                                 -> (lastChange nfb, f (val nfb))
                   syncIO $ putMVar m (Just (i,v))
                   nfv <- updateGetNF v
                   (same,pr) <- if not (same nfv)
                                then case mnf of
                                      Just pnf | lastChange pnf == lastChange nfv -> return (False, val pnf)
                                      _    -> (False,) . prev <$> updateGetNF (f (prev nfb))
                               else return (same nfv,prev nfv)
                   return $ BNF (max (lastChange nfb) (lastChange nfv))
                                (val nfv) pr same (switches nfb >< switches nfv)

  WhenJust b -> do nf <- updateGetNF b
                   ev <- case val nf of
                     Just x  -> return $ return x
                     Nothing -> join <$> planFirst ((curIO self <$) <$> toList (switches nf))
                   let (s,prEv) = if same nf
                                  then (True,ev)
                                  else case prev nf of 
                                           Just x  -> (False, return x)
                                           Nothing -> (True ,ev)
                   return nf {val = ev, prev = prEv, same=s }
{-
  SeqB a b ->  do BNF ta av ap sa <- updateGetNF a 
                  BNF tb bv bp sb <- updateGetNF b
                  return $ BNF (max ta tb) (av `seq` bv) (ap `seq` bp) (sa >< sb)
-}

-- rewriteBehaviour :: Behaviour a -> Now (Behaviour a)
rewriteBehaviour :: BehaviourSyntax a -> Now (BehaviourSyntax a)
rewriteBehaviour = \case 
  Bnd b m f    -> getConstant b >>= \case 
                    Just (t,a) -> updateRewrite $ delay t (f a)
                    Nothing    -> return $ Bnd b m f
  Delay t b -> updateRewrite b >$< \case
                Delay t' b' -> Delay (max t t') b'
                _           -> Delay t b
  Switched i b v -> getRound >$< \j ->
                           if j > i 
                           then Delay i v
                           else Switched i b v
  Switch b e -> evNow e >>= return . \case 
                 Just (i,v)  -> Switched i b v
                 Nothing     -> Switch b e
  e          -> return e


-- end non-shared behaviour


      
-- start shared behaviour

data BehaviourSyntax a where
 Const      :: a -> BehaviourSyntax a
 MFix       :: (a -> Behaviour a) -> BehaviourSyntax a
 Delay      :: Time -> Behaviour a -> BehaviourSyntax a
 Switch     :: Behaviour a -> Event (Behaviour a) -> BehaviourSyntax a
 Switched   :: Time -> Behaviour a -> Behaviour a -> BehaviourSyntax a
 Bnd        :: Behaviour a -> MVar (Maybe (Time,Behaviour b)) -> (a -> Behaviour b) -> BehaviourSyntax b
 BeforeSwitch :: Behaviour a -> BehaviourSyntax a
 WhenJust   :: Behaviour (Maybe a) -> BehaviourSyntax (Event a)
 SeqB       :: Behaviour x -> Behaviour a -> BehaviourSyntax a

newtype Behaviour a = B (MVar (Time,BehaviourSyntax a, Maybe (BehaviourNF a)))

newBehaviour :: BehaviourSyntax a -> Behaviour a
newBehaviour s = B $ unsafePerformIO $ newMVar (minBound, s, Nothing)
{-# NOINLINE newBehaviour #-}

instance Monad Behaviour where
  return x = newBehaviour (Const x)
  m >>= f = newBehaviour (Bnd m (unsafePerformIO $ newMVar Nothing) f)
  {-# NOINLINE (>>=) #-}

instance MonadFix Behaviour where
  mfix f = newBehaviour (MFix f)


beforeSwitch b = newBehaviour (BeforeSwitch b)
switch b e = newBehaviour (Switch b e)
whenJust b =  newBehaviour (WhenJust b)
seqB l r = newBehaviour (SeqB l r)
delay t b = newBehaviour (Delay t b)

-- end shared behaviour

instance Functor Behaviour where
  fmap = liftM

instance Applicative Behaviour where
  pure = return
  (<*>) = ap


