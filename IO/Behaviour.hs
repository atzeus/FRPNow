{-# LANGUAGE GADTs, TupleSections,FlexibleInstances,UndecidableInstances, TypeFamilies,FlexibleContexts #-}

module IO.Behaviour where

import Control.Applicative
import Control.Monad.IO.Class
import System.IO.Unsafe
import Data.IORef
import IO.Event

data BehaviourTerm s a where
  Hold   :: a -> BehaviourTerm s a
  Ap     :: Behaviour s (a -> b) -> Behaviour s a -> BehaviourTerm s b
  Switch :: Behaviour s a -> Event s (Behaviour s a) -> BehaviourTerm s a
   
data BehaviourState s a = 
    BS { lastUpdate :: TimeStamp,
         term       :: BehaviourTerm s a,
         curVal     :: a }

data Behaviour s a = B (IORef BehaviourState)

instance Functor (Behaviour s) where fmap = (<$>)

{-# NOINLINE newBehaviour #-}
newBehaviourState :: BehaviourTerm s a -> BehaviourState s a 
newBehaviourState t = BS minBound t undefined 

newBehaviour t = unsafePerformIO $
   do r <- newIORef (newBehaviourState t)
      return (B r)

instance Applicative (Behaviour s) where
  pure a  = newBehaviour $ Hold a
  f <*> x = newBehaviour $ Ap f x

switch :: Behaviour s a -> Event s (Behaviour s a) -> Behaviour s a
switch b e = newBehaviour $ Switch b e


getBehaviour :: TimeStamp -> Behaviour s a -> IO a
getBehaviour t b@(B r) = 
  do updateBehaviour t b 
     getVal b

updateBehaviour t b@(B r) =
  do tr <- getTerm b
     case tr of
      Hold a -> writeIORef r (maxBound, undefined, a)
      Ap f x -> 
        do updateBehaviour f
           updateBehaviour x
           ft <- getTerm f
           xt <- getTerm x
           case (ft,xt) of
             (Hold fv, Hold xv) -> writeIORef r (maxBound, undefined, fv xv)
             _                  -> do fv <- getVal ft
                                      xv <- getVal xt
                                      writeIORef r (t, tr, fv xv)
      Switch b e ->

       


getTerm :: Behaviour s a -> IO (BehaviourTerm s a)
getTerm (B r) = do (_,t,_) <- readIORef r; return t

getVal :: Behaviour s a -> IO a
getVal (B r) = do (_,_,a) <- readIORef r; return a

