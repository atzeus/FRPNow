module IOEv where

import TermM
import ConcFlag
import Control.Concurrent

type TimeStamp = Int64 

data Improving a = NotYet TimeStamp | Occured a

data Event s a = E { time :: (IORef (Improving a), val :: IORef (Maybe a)}

type PlanIO s = TermM (PlanIOPrim s)
data PlanIOPrim s a where
	Act  :: IO a -> PlanIOPrim s a
	Plan :: Event s (PlanIO s a) -> PlanIOPrim s (Event s a)

runPlanIO :: (forall s. PlanIO s (Event s a)) -> IO a
runPlanIO 

data Planned s = forall a. Planned (Event s (PlanIO s a)) (Event s a)

type PIOState s a = PIOState { 	curTime' :: TimeStamp,
                    		   	plans' :: [Planned],
                    		   	end'   :: Event s a,
                    		   	flag'  :: Flag }

type PIOM s a = StateT (PIOState s a) 

runPlanIO' :: PlanIO s a -> PIOM s x a
runPlanIO' t = case viewTermM t of
	Return a -> return a
	p :>>= f -> case p of
		Act m -> startIO >>= runPlanIO . f
		Plan e -> 

plan :: Event s (PlanIO s a) -> PIOM s x (Event s a)
plan e = do e@(E im r) <- newEvent 
			n <- getEv e
			case n of
				Just a  -> runPlanIO' a >>= writeIORef r 
			 	Nothing -> planLater a
			return e


curTime :: PIOM s x (TimeStamp)
curTime = curTime' <$> get

getFlag :: PIOM s x Flag
getFlag = flag' <$> get

getEv :: Event s a -> PIOM s x (Maybe a)
getEv e@(E ir _) = updateEv e >> impToMaybe <$> readIORef ir
	where impToMaybe (Occured a) = Just a
		  impToMaybe (NotYet _)  = Nothing	

updateEv :: Event s a -> PIOM s a IO ()
updateEv (E ir vr) = 
	do 	i <- lift $ readIORef ir
		case i of
			Occured a -> return ()
			NotYet p  -> do t <- curTime
							if t /= p
							then return () 
							else	v <- lift $ readIORef vr
									case v of
										Just a  -> lift $ writeIORef ir (Occured a)
										Nothing -> lift $ writeIORef ir (NotYet t)


plan :: Event s (PlanIO s a) -> PIOM s x (Event s a)
plan e =  

startIO :: IO a -> PIOM (Event s a)
startIO a = do 	e@(E im r) <- newEvent
				f <- getFlag
				lift $ sparkIO f r a  
				return e

newImproving :: PIOM s x (IORef Improving)
newImproving = do t <- curTime
				  lift $ newIORef (NotYet t)

sparkIO :: Flag -> IORef (Maybe a) -> IO a -> IO ()
sparkIO f r m = do	i <- myThreadId
					i2 <- forkIO
					if i == i2
					then  do v <- a; putIORef r (Just v); signal f
					else return () 

newEvent :: PIOM s x (Event s a)
newEvent = do im <- newImproving
			  r <- lift $ newIORef Nothing
			  return (E im r)






