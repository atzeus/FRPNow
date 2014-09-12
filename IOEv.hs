{-# LANGUAGE GADTs, Rank2Types,NamedFieldPuns #-}

module IOEv where

import TermM
import ConcFlag
import Control.Concurrent
import Control.Monad.State.Lazy
import Control.Monad
import Data.IORef 
import Data.Int
import Control.Applicative

type TimeStamp = Int64 

data Improving a = NotYet TimeStamp | Occured a

data Event s a = E { time :: IORef (Improving a), val :: IORef (Maybe a)}

type PlanIO s = TermM (PlanIOPrim s)
data PlanIOPrim s a where
    Act  :: IO a -> PlanIOPrim s (Event s a)
    Plan :: Event s (PlanIO s a) -> PlanIOPrim s (Event s a)

runPlanIO :: (forall s. PlanIO s (Event s a)) -> IO a
runPlanIO p = do f <- newFlag
                 evalStateT (runPlanIO' p >>= runTimeSteps) (PIOState minBound [] f)
                 

runTimeSteps :: Event s a -> PIOM s a
runTimeSteps e = loop where
  loop = do ev <- getEv e 
            case ev of
                Just a -> return a
                Nothing -> do runPlans
                              incTime
                              f <- getFlag 
                              lift $ waitForSignal f
                              loop
                 
runPlans :: PIOM s ()
runPlans = takePlans >>= mapM_ runPlan
    where runPlan :: Planned s -> PIOM s ()
          runPlan p@(Planned e r) = 
            do ev <- getEv e
               case ev of
                    Just n  -> runPlanIO' n >> return ()
                    Nothing -> addPlan p



data Planned s = forall a. Planned (Event s (PlanIO s a)) (IORef (Maybe a))

data PIOState s = PIOState {     curTime' :: TimeStamp, -- write
                                 plans' :: [Planned s], -- write 
                                 flag'  :: Flag } -- read only

type PIOM s = StateT (PIOState s) IO

runPlanIO' :: PlanIO s a -> PIOM s a
runPlanIO' t = case viewTermM t of
    Return a -> return a
    p :>>= f -> handlePrim p >>= runPlanIO' . f 
    where handlePrim :: PlanIOPrim s a -> PIOM s a
          handlePrim (Act m)  = startIO m
          handlePrim (Plan e) = plan e

plan :: Event s (PlanIO s a) -> PIOM s (Event s a)
plan e = do ep@(E im r) <- newEvent 
            n <- getEv e
            case n of
                Just a  -> do v <- runPlanIO' a; lift (writeIORef r (Just v)) ; updateEv ep
                Nothing -> addPlan (Planned e r) 
            return ep


curTime :: PIOM s (TimeStamp)
curTime = curTime' <$> get

getFlag :: PIOM s Flag
getFlag = flag' <$> get

takePlans :: PIOM s [Planned s]
takePlans = do b <- get
               put (b{plans' = []})
               return (plans' b)

addPlan :: Planned s -> PIOM s ()
addPlan p = do  b <- get 
                put b { plans' = p: plans' b }

incTime :: PIOM s ()
incTime =  do b <- get 
              put b { curTime' = (curTime' b) + 1 }

getEv :: Event s a -> PIOM s (Maybe a)
getEv e@(E ir _) = updateEv e >> impToMaybe <$> lift (readIORef ir)
    where impToMaybe (Occured a) = Just a
          impToMaybe (NotYet _)  = Nothing    

updateEv :: Event s a -> PIOM s ()
updateEv (E ir vr) = 
    do  i <- lift $ readIORef ir
        case i of
            Occured a -> return ()
            NotYet p  -> do t <- curTime
                            if t /= p
                            then return () 
                            else do v <- lift $ readIORef vr
                                    case v of
                                        Just a  -> lift $ writeIORef ir (Occured a)
                                        Nothing -> lift $ writeIORef ir (NotYet t)


startIO :: IO a -> PIOM s (Event s a)
startIO a = do  e@(E im r) <- newEvent
                f <- getFlag
                lift $ sparkIO f r a  
                return e

newImproving :: PIOM s (IORef (Improving a))
newImproving = do t <- curTime
                  lift $ newIORef (NotYet t)

sparkIO :: Flag -> IORef (Maybe a) -> IO a -> IO ()
sparkIO f r m = forkIO (do v <- m; writeIORef r (Just v); signal f ; return ()) >> return ()

newEvent :: PIOM s (Event s a)
newEvent = do im <- newImproving
              r <- lift $ newIORef Nothing
              return (E im r)






