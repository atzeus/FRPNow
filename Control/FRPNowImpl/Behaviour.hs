
{-# LANGUAGE ScopedTypeVariables, LambdaCase #-}
module Control.FRPNowImpl.Behaviour where
import Control.Applicative hiding (empty,Const)
import Control.Monad
import Control.Monad.Fix
import Control.Concurrent.MVar
import System.IO.Unsafe
import Control.FRPNowImpl.NowTime
import Control.FRPNowImpl.NewEvent
import Debug.Trace
import Data.Sequence
import Data.Foldable (toList)
import Data.Maybe

infixr 3 :-> 
data BHT a = (:->) { headB :: Now a , tailB :: Event (Behaviour a) }
           | SameAs (Behaviour a) (BHT a) 
           | Const a
   

newtype Behaviour a = B { getHT :: Now (BHT a) }

              
curIO :: Behaviour a -> Now a
curIO b = getHT b >>= headB

instance Monad Behaviour where
  return a = B $ return (return a :-> never)
  m >>= f = memo $ bind m f

bind m f = B $
   do v <- noSameAs <$> getHT m
      case v of
       h :-> t -> do x <- h
                     getHT (f x `switch'` ((`bind` f) <$> t))
       Const x -> let v = f x 
                  in SameAs v <$> getHT v

switch b e = memo $ switch' b e

switch' ::  Behaviour a -> Event (Behaviour a) -> Behaviour a
switch' b e = B  $ 
  getEv e >>= \case 
    Just a  -> SameAs a <$> getHT a
    Nothing -> do h :-> t <- normHT <$> getHT b
                  ts <- firstObsNow t e
                  return $ h :-> ts

whenJust b = memo $ whenJust b

whenJust' :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust' b = B $ 
  noSameAs <$> getHT b >>= \case
      Const x -> return $ Const (maybe never return x)
      h :-> t -> do let tw = whenJust' <$> t
                    h <- constMemo (head h tw)
                    return (h :-> tw)
  where head h tw = h >>= \case 
         Just x  -> return $ pure x
         Nothing -> join <$> planIO (curIO <$> tw)

{-

seqS :: Behaviour x -> Behaviour a -> Behaviour a
seqS l r = B $ 
  do (hl :-> sl) <- getHT l
     (hr :-> sr) <- getHT r
     h  <- constMemo $ hl >> hr
     let t = (l `seqS`) <$> sr
     return $ h :-> t
      
-}



getNowAgain :: BHT a -> Now (BHT a)
getNowAgain (h :-> t) = getEv t >>= \case
      Just x  -> getHT x >>= getNowAgain
      Nothing -> return (h :-> t)


constMemo :: Now a -> Now (Now a)
constMemo n = syncIO $ runMemo <$> newMVar (Left n) where
  runMemo m = do v <- syncIO $ takeMVar m
                 v' <- case v of
                         Left x  -> x 
                         Right x -> return x
                 syncIO $ putMVar m (Right v')
                 return v'


data MemoInfo a = Uninit (Behaviour a) | Init (BHT a) | SameAsS (Behaviour a) | ConstS a

memo :: Behaviour a -> Behaviour a
memo b = B $ unsafePerformIO $ runMemo <$> newMVar (Uninit b) where
  runMemo m = 
     do v <- syncIO $ takeMVar m 
        res <- case v of
                Uninit b  -> getHT b 
                Init m    -> getNowAgain m
                SameAsS b -> SameAs b <$> getHT b
                ConstS x  -> return (Const x)
        let (newState, res') = case normalizeBNF res of
                                SameAs n nf -> (SameAsS n, SameAs n nf)
                                Const x     -> (ConstS x, SameAs (return x) (Const x))
                                nf          -> (Init nf, nf)
        syncIO $ putMVar m newState
        return res'
{-# NOINLINE memo #-}       
noSameAs (SameAs _ (SameAs _ _)) = error "Double same as!"
noSameAs (SameAs _ nf) = nf
noSameAs n             = n

normHT nf = case noSameAs (normalizeBNF nf) of
             Const x -> return x :-> never
             x       -> x

normalizeBNF (SameAs _ (SameAs n nf)) =  normalizeBNF $ SameAs n nf
normalizeBNF (SameAs _ (Const x))     =  Const x
normalizeBNF nf              = nf

instance Functor Behaviour where
  fmap = liftM

instance Applicative Behaviour where
  pure = return
  (<*>) = ap

-- memo
-- const
-- same as


{-
curIO :: Behaviour a -> Now a
curIO b = headB <$> getHT b

instance Monad Behaviour where
  return a = Behaviour (return (ConstB a))
  m >>= f = memo $ bind m f

bind m f =  Behaviour $
   do v <- getHT' m
      case noSameAs (normalizeBNF v) of
       ConstB x -> let v = f x in SameAs v <$> getHT' v
       h :-> t  -> getHT' (f h  `switch'` fmap (>>= f) t)
      


instance MonadFix Behaviour where
  mfix f =   Behaviour $  mfix (getHT . f . headB )

switch b e = memo $ switch' b (sameAs <$> e)



sameAs n = Behaviour $ SameAs n <$> getHT' n

whenJust b = memo (whenJust' b)

whenJust' :: forall a. Behaviour (Maybe a) -> Behaviour (Event a)
whenJust' b = Behaviour $ 
  do h :-> t  <- getHT b
     let tw = fmap whenJust' t
     case h of
         Just x  -> return (pure x :-> tw)
         Nothing -> do tn <- planIO (getHT <$> tw)
                       return $ (tn >>= headB) :-> (tn >>= tailB)

seqS l r = seqS' l r







{-# NOINLINE memo #-}       
noSameAs (SameAs _ (SameAs _ _)) = error "Double same as!"
noSameAs (SameAs _ nf) = nf
noSameAs n             = n


normalizeBNF' (SameAs _ (SameAs n nf)) =  normalizeBNF $ SameAs n nf
normalizeBNF' (SameAs _ (ConstB x))     = ConstB x
normalizeBNF' nf              = nf


normalizeBNF (SameAs _ (SameAs n nf)) =  normalizeBNF $ SameAs n nf
normalizeBNF (SameAs _ (ConstB x))     =  ConstB x
normalizeBNF nf              = nf




instance Functor Behaviour where
  fmap = liftM

instance Applicative Behaviour where
  pure = return
  (<*>) = ap

-}
{-


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
    j  <- getRound
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


-}
