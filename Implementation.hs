
{-# LANGUAGE GADTs, TupleSections, Rank2Types,NamedFieldPuns, TypeSynonymInstances,FlexibleInstances,GeneralizedNewtypeDeriving #-}

module Implementation (TimeStamp,Event, never,  Behaviour, switch, whenJust, planBehaviour, Now, liftBehaviour, act, planNow, runNow,printState) where

import System.IO.Unsafe
import Data.IORef 
import Data.Int
import Control.Concurrent
import Control.Applicative 
import Control.Monad
import Control.Monad.State.Lazy
import System.IO
import Control.Monad.Fix
import System.Mem.Weak
import Data.Maybe
import Debug.Trace

import Util.TermM
import Util.ConcFlag
import Util.SingleWriteIORef

--- public interface -----------

instance Functor (Event s)  where
  fmap f e = newEvent (FMap f e)

instance Applicative (Event s) where
  pure = return
  (<*>) = ap


instance Monad (Event s) where
  return = newEvent . Always
  m >>= f = ejoin $ fmap f m

never :: Event s a
never = newEvent Never


instance Functor (Behaviour s)  where
  fmap f m = pure f <*> m

instance Applicative (Behaviour s) where
  pure = return
  (<*>) = ap

planBehaviour e = newBehaviour $ Plan e

instance Monad (Behaviour s) where
  return a = newBehaviour $ Hold a
  m >>= f  = newBehaviour $ BBind m f

instance MonadFix (Behaviour s) where
  mfix f = newBehaviour (FixB f)



planNow p  = Now $ prim (PlanNow p)

switch :: Behaviour s a -> Event s (Behaviour s a) -> Behaviour s a
switch b e = newBehaviour $ Switch b e


printState :: Show a => Behaviour s a -> Now s ()
printState b = Now $ prim (PrintState b)

whenJust b = newBehaviour $ WhenJust b

liftBehaviour b = Now $ prim (LiftB b)
act :: IO a -> Now s (Event s a)
act m   = Now $ prim (Act m)




instance MonadFix (Now s) where
  mfix f = Now $ prim (Fix f)




----------- Events implementation ------------------


type TimeStamp = Int64 -- this is a big place!

data Event s a = E (IORef (EventState s a)) 

data EventState s a 
    = ES { lastUpdateE :: TimeStamp,
           termE       :: EventTerm s a }
    | Oc a

data EventTerm s a where
  Never          ::                             EventTerm s a
  Always         :: a                        -> EventTerm s a
  FMap           :: (a -> b) -> Event s a    -> EventTerm s b 
  Join           :: Event s (Event s a)      -> EventTerm s a
  WatchRef       :: SIORef a                 -> EventTerm s a
  WatchBehaviour :: Behaviour s (Maybe a)    -> EventTerm s a -- make root!
  PlannedNow     :: Event s (Now s a)        -> EventTerm s a -- make root!!
  Planned        :: Event s (Behaviour s a)  -> EventTerm s a -- make root!!
  SameAsE        :: Event s a                -> EventTerm s a



initEventState :: EventTerm s a -> EventState s a 
initEventState t = ES minBound t

{-# NOINLINE newEvent #-}
newEvent t = unsafePerformIO $ newEventIO t 
  

newEventIO t =  do r <- newIORef (initEventState t)
                   return (E r)

ejoin e = newEvent (Join e)

watchRef :: SIORef a -> Event s a
watchRef r = newEvent (WatchRef r)

isNever :: Event s a ->  PIOM s Bool
isNever e@(E r) = 
   do getEv e
      rv <- lift $ readIORef r
      return (isNever rv)
  where 
    isNever (ES _ Never) = True
    isNever _       = False

getEv :: Event s a -> PIOM s (Maybe a)
getEv (E r) = 
  do tryUpdateE r
     rv <- lift $ readIORef r
     return (getVal rv)
  where 
    getVal (Oc a) = Just a
    getVal _      = Nothing

tryUpdateE :: IORef (EventState s a) -> PIOM s ()
tryUpdateE r = 
 do rv <- lift $ readIORef r
    case rv of
      Oc a    -> return ()
      ES tu e -> 
        do t <- curTime 
           if tu >= t 
           then return ()
           else do -- if an event depends on its own occurance, it will not occur now 
                   lift $ writeIORef r (ES t e) 
                   ev <- updateE t e 
                   lift $ writeIORef r ev
                   lift $ flattenRefsE r

updateE :: TimeStamp -> EventTerm s a -> PIOM s (EventState s a)
updateE t e = case e of
    Never      -> return (ES maxBound Never)
    Always a   -> return (Oc a)
    FMap f ed  -> do v <- getEv ed
                     case v of
                      Just a  -> return (Oc (f a))
                      Nothing -> do n <- isNever ed
                                    if n 
                                    then return (ES maxBound Never)
                                    else return (ES t e)
    Join ed    -> do v <- getEv ed
                     case v of
                      Just e2 -> do r <- getEv e2
                                    case r of
                                       Just a -> return (Oc a)
                                       _      -> return (ES t (SameAsE e2))
                      Nothing -> do n <- isNever ed
                                    if n 
                                    then return (ES maxBound Never)
                                    else return (ES t e)
    WatchRef w -> do v <- lift $ readSIORef w
                     case v of
                      Just a  -> return (Oc a)
                      Nothing -> return (ES t e)
    WatchBehaviour b -> do bv <- getBehaviour b
                           case bv of
                             Just a  -> return (Oc a)
                             Nothing -> do n <- isConstant b
                                           if n 
                                           then return (ES maxBound Never)
                                           else return (ES t e)
    PlannedNow e2 -> do m <- getEv e2
                        case m of
                           Just (Now t) -> do a <- runPlanIO' t
                                              return (Oc a)
                           Nothing -> do n <- isNever e2
                                         if n 
                                         then return (ES maxBound Never)
                                         else return (ES t e)
    Planned e2 -> do m <- getEv e2
                     case m of
                      Just b -> do a <- getBehaviour b
                                   return (Oc a)
                      Nothing -> do n <- isNever e2
                                    if n 
                                    then return (ES maxBound Never)
                                    else return (ES t e)
    SameAsE e -> do ev <- getEv e
                    case ev of
                      Just a -> return (Oc a)
                      _    -> do n <- isNever e
                                 if n 
                                 then return (ES maxBound Never)
                                 else return (ES t (SameAsE e))

flattenRefsE :: IORef (EventState s a) -> IO ()
flattenRefsE r = 
  do rv <- readIORef r
     case rv of
       ES _ (SameAsE (E r2)) -> 
        do rv2 <- readIORef r2
           case rv2 of
            ES _ (SameAsE (E r3)) -> writeIORef r rv2 
            _ -> return ()
       _ -> return ()


---------- Behaviour implementation ----------------------------


data BehaviourTerm s a where
  Hold     :: a                                         -> BehaviourTerm s a
  BBind    :: Behaviour s a -> (a -> Behaviour s b)     -> BehaviourTerm s b
  Switch   :: Behaviour s a -> Event s (Behaviour s a)  -> BehaviourTerm s a
  WhenJust :: Behaviour s (Maybe a)                     -> BehaviourTerm s (Event s a)
  Plan     :: Event s (Behaviour s a)                   -> BehaviourTerm s (Event s a)
  FixB     :: (a -> Behaviour s a)                      -> BehaviourTerm s a
  SameAsB  :: Behaviour s a                             -> BehaviourTerm s a


data BehaviourState s a = 
    BS { lastUpdateB :: TimeStamp,
         termB       :: BehaviourTerm s a,
         curVal     :: a }


data Behaviour s a = B (IORef (BehaviourState s a))


newBehaviourState :: BehaviourTerm s a -> BehaviourState s a 
newBehaviourState t = BS minBound t undefined 

{-# NOINLINE newBehaviour #-}
newBehaviour t = unsafePerformIO $
   do r <- newIORef (newBehaviourState t)
      return (B r)

isConstant :: Behaviour s a ->  PIOM s Bool
isConstant b@(B r) = 
   do getBehaviour b
      rv <- lift $ readIORef r
      return (isConstant rv)
  where 
    isConstant (BS t _ _) = t == maxBound

getBehaviour :: Behaviour s a -> PIOM s a
getBehaviour (B r) = 
  do tryUpdateB r
     BS _ _ a <- lift $ readIORef r
     return a

tryUpdateB :: IORef (BehaviourState s a) -> PIOM s ()
tryUpdateB r = 
  do t <- curTime 
     BS tu e _ <- lift $ readIORef r
     if tu < t 
     then do e' <- updateB t e   
             lift $ writeIORef r e'
             lift $ flattenRefsB r
     else return ()
          
updateB :: TimeStamp -> BehaviourTerm s a -> PIOM s (BehaviourState s a)
updateB t e = case e of
    Hold   a   -> return (BS maxBound e a)
    BBind m f   -> do mv <- getBehaviour m
                      let x = f mv
                      v <- getBehaviour x
                      mc <- isConstant m
                      if mc 
                       then return (BS t (SameAsB x) v)
                       else return (BS t (BBind m f) v)
    Switch b ev -> do evv <- getEv ev
                      case evv of
                       Just b' -> do bv <- getBehaviour b'
                                     return (BS t (SameAsB b') bv)
                       Nothing -> do v <- getBehaviour b
                                     n <- isNever ev
                                     if n
                                     then return (BS t (SameAsB b) v)
                                     else return (BS t e v)

    Plan ev -> do getEv ev
                  evv <- lift $ newEventIO $ Planned ev
                  addWeakRoot (RootE ev)
                  return (BS t e evv)
    WhenJust b -> do getBehaviour b
                     ev <- lift $ newEventIO $ WatchBehaviour b
                     addWeakRoot (RootE ev)
                     return (BS t e ev)
    FixB f    ->  do v <- mfix (getBehaviour . f)
                     return (BS t (FixB f) v)
    SameAsB b -> do bv <- getBehaviour b
                    return (BS t e bv)

flattenRefsB :: IORef (BehaviourState s a) -> IO ()
flattenRefsB r = 
  do rv <- readIORef r
     case rv of
       BS _ (SameAsB (B r2)) _ -> 
        do rv2 <- readIORef r2
           case rv2 of
            BS _ (SameAsB (B r3)) _ -> do writeIORef r rv2 
            _ -> return ()
       _ -> return ()

------------- Now implementation -----------------------



newtype Now s a = Now { getNow :: TermM (PlanIOPrim s) a } deriving (Functor,Applicative,Monad)

data PlanIOPrim s a where
    LiftB       :: Behaviour s a          -> PlanIOPrim s a
    Fix         :: (a -> Now s a)         -> PlanIOPrim s a
    Act         :: IO a                   -> PlanIOPrim s (Event s a)
    PlanNow     :: Event s (Now s a)      -> PlanIOPrim s (Event s a)

    -- debug thing
    PrintState  :: Show a => Behaviour s a -> PlanIOPrim s ()

runNow :: (forall s. Now s (Event s a)) -> IO a
runNow (Now p) = 
    do f <- newFlag
       evalStateT 
         (do e <- runPlanIO' p ;  runTimeSteps e)  
         (initState f)
  where initState f = PIOState (minBound + 1) [] f
                 

runTimeSteps :: Event s a -> PIOM s a
runTimeSteps e = loop where
  loop = do incTime
            ev <- getEv e 
            case ev of
                Just a -> return a
                Nothing -> 
                  do runRoots
                     f <- getFlag 
                     lift $ waitForSignal f
                     loop
                 
runRoots :: PIOM s ()
runRoots = takeRoots >>= mapM_ tryRunRoot
    where tryRunRoot r@(WeakR w) = do m <- lift $ deRefWeak w
                                      case m of
                                        Just x -> runRoot x >>= reAdd r          
                                        Nothing -> return ()
          tryRunRoot r@(StrongR w) = runRoot w >>= reAdd r
          reAdd w True = addRoot w
          reAdd _ False = return ()
          runRoot :: Root s -> PIOM s Bool
          runRoot (RootE e) = 
            do ev <- getEv e
               case ev of
                    Just n  -> return False
                    Nothing -> return True
          runRoot p@(RootB e) = getBehaviour e >> return True
              




runPlanIO' :: TermM (PlanIOPrim s) a-> PIOM s a
runPlanIO' t = case viewTermM t of
    Return a -> return a
    p :>>= f -> handlePrim p >>= runPlanIO' . f 
    where handlePrim :: PlanIOPrim s a -> PIOM s a
          handlePrim (LiftB b)      = getBehaviour  b
          handlePrim (Fix f)        = mfix (runPlanIO' . getNow . f)
          handlePrim (PrintState b) = getBehaviour b >> (lift $ printStateBB b)
          handlePrim (Act m)        = startIO m
          handlePrim (PlanNow e)    = plan' e

plan' :: Event s (Now s a) -> PIOM s (Event s a)
plan' e = do let e' = newEvent (PlannedNow e)
             getEv e'
             addStrongRoot (RootE e')
             return e'



data Ref a = StrongR a
           | WeakR (Weak a) 


data Root s = forall a. RootE (Event s a)
                | forall a. RootB (Behaviour s a)

data PIOState s = PIOState {     curTime' :: TimeStamp, -- write
                                 roots  :: [Ref (Root s)], -- write
                                 flag'  :: Flag } -- read only

type PIOM s = StateT (PIOState s) IO

startIO :: IO a -> PIOM s (Event s a)
startIO a = 
  do r <-  lift $ newSIORef
     f <- getFlag
     lift $ forkIO $ 
            do v <- a
               putSIORef r v
               signal f
     return (watchRef r)


curTime :: PIOM s TimeStamp
curTime = get >>= return . curTime'

getFlag :: PIOM s Flag
getFlag = get >>= return .  flag' 

takeRoots :: PIOM s [Ref (Root s)]
takeRoots =
 do b <- get
    put (b{roots = []})
    return (roots b)

addRoot :: Ref (Root s) -> PIOM s ()
addRoot w = 
  do b <- get 
     put b { roots = w : roots b }

addStrongRoot :: Root s -> PIOM s ()
addStrongRoot w = addRoot (StrongR w)

addWeakRoot :: Root s -> PIOM s ()
addWeakRoot w = 
  do ww <- lift $ mkWeak w w Nothing
     addRoot (WeakR ww)


incTime :: PIOM s ()
incTime =  do b <- get 
              put b { curTime' = (curTime' b) + 1 }








------- Debug stuff -----------------
printErr :: String -> PIOM s ()
printErr s = lift $ hPutStrLn stderr s

showE :: EventTerm s a -> IO ()
showE (Never) = putStr "never"
showE (Always x ) = putStr "always"
showE (FMap _ e) = do putStr "fmap(" 
                      printEvent e 
                      putStr ")"
showE (Join  e) =  do putStr "join(" 
                      printEvent e 
                      putStr ")"
showE (WatchRef r) = putStr $ "watchingref(" ++ show r ++ ")" 

showE (WatchBehaviour b) = 
                    do putStr "watching(" 
                       printStateB b
                       putStr ")"
showE (Planned b) = do putStr "plan(" 
                       printEvent b
                       putStr ")"
showE (SameAsE b) = do putStr "sameAs(" 
                       printEvent b
                       putStr ")"
  
showES :: EventState s a -> IO ()
showES (ES u t) = do --putStr "(" 
                     putStr (show u) 
                     --putStr "->"
                     showE t
                     --putStr ")"
showES (Oc _)  = putStr "Oc"

printEvent :: Event s a -> IO ()
printEvent (E r) = do v <- readIORef r
                      showES v


showB ::  BehaviourTerm s a -> IO ()
showB (Hold _) = putStr "hold"
showB (BBind m _) = do putStr "bind(" 
                       printStateB m 
                       putStr ")"
showB (Switch m e) = do putStr "switch(" 
                        printStateB m 
                        putStr ","
                        printEvent e
                        putStr ")"
showB (SameAsB b) = do putStr "sameAs(" 
                       printStateB b
                       putStr ")"
showBs ::  Show a => BehaviourTerm s a -> IO ()
showBs (Hold _) = putStr "hold"
showBs (BBind m _) = do putStr "bind(" 
                        printStateB m 
                        putStr ")"
showBs (Switch m e) = do putStr "switch(" 
                         printStateBB m 
                         putStr ","
                         printEvent e
                         putStr ")"
showBs (SameAsB b) = do putStr "sameAs(" 
                        printStateBB b
                        putStr ")"
  
showBB :: Show a => BehaviourState s a -> IO ()
showBB b@ (BS u t v) = do showBs t
                          putStrLn ""
                          putStrLn (show v)

showBS :: BehaviourState s a -> IO ()
showBS (BS u t _) = do --putStr "(" 
                       putStr (show u) 
                       --putStr "->"
                       showB t
                       --putStr ")"

printStateBB :: Show a => Behaviour s a -> IO ()
printStateBB (B r) = do v <- readIORef r
                        showBB v

printStateB ::  Behaviour s a -> IO ()
printStateB (B r) = do v <- readIORef r
                       showBS v

