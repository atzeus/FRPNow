{-# LANGUAGE LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}

module Syntactic.Behaviour(Behaviour,switch, whenJust,seqB,cur) where

import Data.Sequence
import Control.Applicative hiding (empty,Const)
import Syntactic.Time
import Data.Foldable
import Control.Monad
import System.IO.Unsafe
import Control.Concurrent.MVar

{-
-- start non-shared behaviour

data Behaviour a where
 Const      :: a -> Behaviour a
 Switch     :: Behaviour a -> Event (Behaviour a) -> Behaviour a
 Bnd        :: Behaviour a -> (a -> Behaviour b) -> Behaviour b
 WhenJust   :: Behaviour (Maybe a) -> Behaviour (Event a)
 SeqB       :: Behaviour x -> Behaviour a -> Behaviour a

instance Monad Behaviour where
  return = Const
  (>>=)  = Bnd



switch = Switch
whenJust = WhenJust

type BehaviourNF a = (a, Seq (Event ()))

cur :: Behaviour a -> Now a
cur m = fst <$> getNormalForm m

getNormalForm :: Behaviour a -> Now (BehaviourNF a)
getNormalForm e = case e of
  Const x -> return (x,empty)

  Switch b e -> evNow e >>= \case 
                     Just x  -> getNormalForm x
                     Nothing -> (\(x,l) -> (x, fmap (const ()) e <| l)) <$> getNormalForm b

  Bnd b f -> do (a,sa) <- getNormalForm b 
                (b,sb) <- a `seq` getNormalForm (f a)
                return (b,sa >< sb) 

  WhenJust b -> do (a,s) <- getNormalForm b
                   ev <- case a of
                     Just x  -> return $ return x
                     Nothing -> let again = fmap $ const $ cur e
                                in join <$> planFirst (fmap again (toList s))
                   return (ev,s)

  SeqB a b ->  do (av,_ ) <- getNormalForm a 
                  (bv,sb) <- getNormalForm b
                  return (av `seq` bv, sb)


-- end non-shared behaviour
-}

      
-- start shared behaviour

data SwitchState a = Before { curB :: Behaviour a, ev :: Event (Behaviour a) }
                   | After  { curB :: Behaviour a }


data BehaviourSyntax a where
 Const    :: a -> BehaviourSyntax a
 Switch   :: MVar (SwitchState a) -> BehaviourSyntax a
 Bnd      :: Behaviour a -> (a -> Behaviour b) -> BehaviourSyntax b
 SameAs   :: Behaviour a -> BehaviourSyntax a
 WhenJust :: Behaviour (Maybe a) -> BehaviourSyntax (Event a)
 SeqB     :: Behaviour x -> Behaviour a -> BehaviourSyntax a


data Behaviour a = B (BehaviourSyntax a) (MVar (Maybe (RoundNr,BehaviourNF a)))

newBehaviour :: BehaviourSyntax a -> Behaviour a
newBehaviour s = unsafePerformIO $ B s <$> newMVar Nothing
{-# NOINLINE newBehaviour #-}

switch b e = newBehaviour $ Switch $ unsafePerformIO $ newMVar $ Before b e
{-# NOINLINE switch #-}
whenJust b = newBehaviour (WhenJust b)
seqB x b   = newBehaviour (SeqB x b)

instance Monad Behaviour where
  return  = newBehaviour . Const
  m >>= f = newBehaviour (Bnd m f)

type BehaviourNF a = (a, Seq (Event ()))


cur :: Behaviour a -> Now a
cur m = fst <$> getNormalForm m

getNormalForm :: Behaviour a -> Now (BehaviourNF a)
getNormalForm bh@(B s m) = 
  do v <- syncIO $ takeMVar m
     j <- getRound
     nf <- case v of
      Just (i,nf@(a,evs)) 

      --  | i == j    -> return nf
        | otherwise -> checkAll (toList evs) >>= \case 
                        Just _  -> getNF s
                        Nothing -> return nf
      _             -> getNF s
     syncIO $ putMVar m (Just (j,nf))
     return nf  where
 getNF e =  case e of
  Const x -> return (x,empty)


  Switch sm -> do trySwitch sm
                  st <- syncIO $ readMVar sm
                  nf <- case st of
                     Before b e -> (\(x,l) -> (x, fmap (const ()) e <| l)) <$> getNormalForm b
                     After b    -> getNormalForm b
                  normalizeSwitch sm
                  return nf
                  

  Bnd b f -> do (a,sa) <- getNormalForm b 
                (b,sb) <- a `seq` getNormalForm (f a)
                return (b,sa >< sb) 

  WhenJust b -> do (a,s) <- getNormalForm b
                   ev <- case a of
                     Just x  -> return $ return x
                     Nothing -> let again = fmap $ const $ cur bh
                                in join <$> planFirst (fmap again (toList s))
                   return (ev,s)

  SeqB a b ->  do (av,_ ) <- getNormalForm a 
                  (bv,sb) <- getNormalForm b
                  return (av `seq` bv, sb)

trySwitch :: MVar (SwitchState a) -> Now ()
trySwitch m = 
  do st <- syncIO $ takeMVar m
     st' <- case st of
       Before b e -> evNow e >>= \case 
        Nothing -> return st
        Just x  -> return $ After x
       After x  -> return $ After x
     syncIO $ putMVar m st'

normalizeSwitch :: MVar (SwitchState a) -> Now ()
normalizeSwitch m = 
  do st <- syncIO $ takeMVar m
     st' <- case st of
       Before b e -> return st
       After x    -> After <$> getDeepestSwitch x 
     syncIO $ putMVar m st'


getDeepestSwitch b@(B s _) =
 do case s of
     Switch m -> syncIO (readMVar m) >>= \case 
       Before _ _ -> return b
       After x -> getDeepestSwitch x
     _ -> return b
      
-- end shared behaviour

instance Functor Behaviour where
  fmap = liftM

instance Applicative Behaviour where
  pure = return
  (<*>) = ap


