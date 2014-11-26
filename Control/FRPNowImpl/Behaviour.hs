{-# LANGUAGE LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}

module Control.FRPNowImpl.Behaviour(Behaviour,switch, whenJust,seqB,curIO) where

import Data.Sequence
import Control.Applicative hiding (empty,Const)
import Control.FRPNowImpl.Event
import Data.Foldable
import Control.Monad
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
data BehaviourNF a = BNF { lastChange :: Time, val :: a, switches ::  Seq (Event () ) }

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

getShape :: Behaviour a -> Now String
getShape (B m) = 
         do (_, s, _) <- syncIO $ readMVar m
            case s of
               Const _ -> return ""
               Delay _ t -> ('D' :) <$> getShape t
               Bnd m f   -> ('B' :) <$> getShape m 
               Switch m e -> ('S' :) <$> getShape m
               WhenJust b -> getShape b

getBehaviour :: Behaviour a -> Now (BehaviourSyntax a,BehaviourNF a)
getBehaviour (B m) = 
 do (i, s, mnf) <- syncIO $ takeMVar m
    j  <- Time <$> getRound
    (s',nf) <- maybeUpdate i s mnf j
    syncIO $ putMVar m (j,s', Just nf)
    return (s',nf) where

  maybeUpdate i s mnf j = case mnf of
    Just nf 
      | i == j -> return (s,nf)
      | otherwise -> checkAll (toList $ switches nf) >>= \case 
           Just (_,a) -> update s
           Nothing    -> return (s,nf)
    Nothing -> update s

  update s = do s' <- rewriteBehaviour s
                nf' <- getNF (B m) s'
                return (s',nf')

-- non-shared : updateRewrite :: Behaviour a -> Now (Behaviour a)
updateRewrite :: Behaviour a -> Now (BehaviourSyntax a)
updateRewrite e = fst <$> getBehaviour e


updateGetNF :: Behaviour a -> Now (BehaviourNF a)
updateGetNF e = snd <$> getBehaviour e



-- getNF :: Behaviour a -> Behaviour a -> Now (BehaviourNF a)
getNF :: Behaviour a -> BehaviourSyntax a -> Now (BehaviourNF a)
getNF self e = case e of
  Const x    -> return $ BNF minBound x empty

  Delay t b  -> do nf <- updateGetNF b
                   return nf {lastChange = max t (lastChange nf)}

  Switch b e -> do nf <- updateGetNF b
                   let e' = () <$ e
                   return nf {switches = e' <| switches nf }

  Bnd b f    -> do BNF tb v sb <- updateGetNF b
                   BNF tf fv sf <- updateGetNF (f v)
                   return $ BNF (max tb tf) fv (sb >< sf)

  WhenJust b -> do nf <- updateGetNF b
                   ev <- case val nf of
                     Just x  -> return $ return x
                     Nothing -> join <$> planFirst ((curIO self <$) <$> toList (switches nf))
                   return nf {val = ev }
  SeqB a b ->  do BNF ta av sa <- updateGetNF a 
                  BNF tb bv sb <- updateGetNF b
                  return $ BNF (max ta tb) (av `seq` bv) (sa >< sb)


-- rewriteBehaviour :: Behaviour a -> Now (Behaviour a)
rewriteBehaviour :: BehaviourSyntax a -> Now (BehaviourSyntax a)
rewriteBehaviour = \case 
  Bnd m f    -> getConstant m >>= \case 
                    Just (t,a) -> updateRewrite $ delay t (f a)
                    Nothing    -> return $ Bnd m f
  Delay t b -> updateRewrite b >$< \case
                Delay t' b' -> Delay (max t t') b'
                _           -> Delay t b
  Switch b e -> evNow e >>= return . \case 
                 Just (i,v)  -> Delay i v
                 Nothing     -> Switch b e
  e          -> return e


-- end non-shared behaviour


      
-- start shared behaviour

data BehaviourSyntax a where
 Const      :: a -> BehaviourSyntax a
 Delay      :: Time -> Behaviour a -> BehaviourSyntax a
 Switch     :: Behaviour a -> Event (Behaviour a) -> BehaviourSyntax a
 Bnd        :: Behaviour a -> (a -> Behaviour b) -> BehaviourSyntax b
 WhenJust   :: Behaviour (Maybe a) -> BehaviourSyntax (Event a)
 SeqB       :: Behaviour x -> Behaviour a -> BehaviourSyntax a

newtype Behaviour a = B (MVar (Time,BehaviourSyntax a, Maybe (BehaviourNF a)))

newBehaviour :: BehaviourSyntax a -> Behaviour a
newBehaviour s = B $ unsafePerformIO $ newMVar (minBound, s, Nothing)

instance Monad Behaviour where
  return x = newBehaviour (Const x)
  m >>= f = newBehaviour (Bnd m f)

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


