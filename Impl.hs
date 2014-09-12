{-# LANGUAGE GADTs #-}

module Impl where

import TermM
import Data.IORef
import Data.Set
import Data.Unique
import Control.Applicative
import Control.Monad
import System.IO.Unsafe
import System.Mem.Weak
 
data Behaviour a = B (IORef (Either (BehaviourTerm a) (IORef a)) -- double IORef -- point to and take over after switch
		-- of switched to behaviour

data BehaviourTerm a where
	BRet   :: a -> BehaviourTerm a
	BBnd   :: Behaviour a -> (a -> Behaviour b) -> BehaviourTerm b
	Switch :: Behaviour a -> Event (Behaviour a) -> BehaviourTerm a

data Event a = E (IORef (Either (EventTerm a) (Maybe a)))

data EventTerm a where
	ERet   :: a -> EventTerm a
	EBnd   :: Event a -> (a -> Event b) -> EventTerm a
	WaitIO :: Unique -> IORef (Maybe a) -> EventTerm a
	WaitBehaviour :: IORef (Maybe a) -> EventTerm a 

type Now a = TermM NowPrim
data NowPrim a where
	Now  :: Behaviour a -> NowPrim a
	Plan :: Event (Now a) -> NowPrim (Event a)
	Act  :: IO a -> NowPrim (Event a)

data PureStateElem where
	-- store names that have now become aliases by switching, remove them when dead
	BSE :: Behaviour a -> BehaviourTerm a -> PureStateElem
	ESE :: Event a     -> EventTerm a     -> PureStateElem

type WeakPSE = Weak PureStateElem

data StateElem where
	PSE :: Weak PureStateElem -> StateElem
	PlannedNow :: Event (Now a) -> StateElem

type State = [StateElem]

nowBehaviour :: Behaviour a -> WriterT State IO a
nowBehaviour b@(B u r) = 
	do 	m <- lift $ readIORef r
		case m of
			Left r -> do (v, t) <- updateBehaviour r
						 lift $ writeIORef r v
						 case t of
						 	Just a -> do 	let se = BSE b t 
						 					wse <- mkWeak se Nothing
						 					tell [wse]
						 	Nothing -> return ()
						 return v 
			Right v -> return v

updateBehaviour :: BehaviourTerm a -> WriterT State IO (a, Maybe BehaviourTerm a)
updateBehaviour b = case b of
	BRet a -> return (a, Nothing)
	BBnd m f -> do 	x <- nowBehaviour m
					v <- nowBehaviour (f x)
					return (v, Nothing)
	Switch m e -> do m <- nowEvent e
					 case m of
					 	Just a -> do x <- nowBehaviour a
					 				 return (x, Nothing)
					 	Nothing -> do x <- nowBehaviour m 
					 				  return (x, Just $ Switch m e)

runNow :: Now a -> WriterT State IO a
runNow a = case viewTermM a of
  Return a -> return a
  p :>>= f -> case p of
  	Now b -> 

{-
runFRP :: Now (Event a) -> IO a
runFRP m = loop m where
	loop (viewTermM -> Return a) = return a
	loop (viewTermM -> p :>>= f) =
		case p of
			Now b -> 
	
-}

instance Functor Behaviour where
	fmap = (<$>)
instance Applicative Behaviour where
	pure = return
	(<*>) = ap
instance Monad Behaviour where
	return a = unsafeStoreBehaviourTerm (BRet a)
	m >>= f  = unsafeStoreBehaviourTerm (BBnd m f)

{-# NOINLINE unsafeStoreBehaviourTerm #-} -- prevent duplication of side effect
unsafeStoreBehaviourTerm :: BehaviourTerm a -> Behaviour a
unsafeStoreBehaviourTerm b = unsafePerformIO $
		do  r <- newIORef (Left b)
			return (B r)
			
{-# NOINLINE unsafeStoreEventTerm #-} -- prevent duplication of side effect
unsafeStoreEventTerm :: EventTerm a -> Event a
unsafeStoreEventTerm b = unsafePerformIO $
		do 	u <- newUnique
			r <- newIORef (Left b)
			return (E u r)

instance Functor Event where
	fmap = (<$>)
instance Applicative Event where
	pure = return
	(<*>) = ap
instance Monad Event where
	return a = unsafeStoreEventTerm (ERet a)
	m >>= f  = unsafeStoreEventTerm (EBnd m f)


