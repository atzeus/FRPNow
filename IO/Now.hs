{-# LANGUAGE GADTs, Rank2Types,NamedFieldPuns #-}

module IO.Now  where

import TermM
import IO.Event
import IO.Behaviour
import ConcFlag
import Control.Monad


act :: IO a -> PlanIO s (Event s a)
act m = prim (Act m)

type Now s = TermM (PlanIOPrim s)
data PlanIOPrim s a where
    Fix        :: (a -> Now s a) -> PlanIOPrim s a
    WhenJust   :: Behaviour s (Maybe a) -> PlanIOPrim s (Event a)
    Now        :: Behaviour s a -> PlanIOPrim s a
    Act        :: IO a -> PlanIOPrim s (Event s a)
    Plan       :: Event s (PlanIO s a) -> PlanIOPrim s (Event s a)

runPlanIO :: (forall s. Now s (Event s a)) -> IO a
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


runPlanIO' :: PlanIO s a -> PIOM s a
runPlanIO' t = case viewTermM t of
    Return a -> return a
    p :>>= f -> handlePrim p >>= runPlanIO' . f 
    where handlePrim :: PlanIOPrim s a -> PIOM s a
          handlePrim (Now b)  = do t <- curTime; getBehaviour t b
          handlePrim (Act m)  = startIO m
          handlePrim (Plan e) = plan' e

plan' :: Event s (PlanIO s a) -> PIOM s (Event s a)
plan' e = do ep@(IOEvent im r) <- newIOEvent 
             n <- getEv e
             case n of
                Just a  -> do v <- runPlanIO' a; lift (writeIORef r (Just v))
                Nothing -> addPlan (Planned e r) 
             return ep


type PIOM s = StateT (PIOState s) IO

data PIOState s = PIOState {     curTime' :: TimeStamp, -- write
                                 plans' :: [Planned s], -- write 
                                 flag'  :: Flag } -- read only

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




