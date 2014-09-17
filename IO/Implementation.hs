
{-# LANGUAGE GADTs, TupleSections, Rank2Types,NamedFieldPuns, TypeSynonymInstances,FlexibleInstances,GeneralizedNewtypeDeriving #-}

module IO.Implementation (TimeStamp,Event, never, seqE, Behaviour, switch, seqB, whenJust, planBehaviour, Now, liftBehaviour, act, planNow, runNow,printState) where

import System.IO.Unsafe
import Data.IORef 
import Data.Int
import Control.Concurrent
import Control.Applicative 
import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Fix
import System.IO
import TermM
import IO.ConcFlag
import IO.SingleWriteIORef
import System.Mem.Weak
import Data.Maybe
import Race
import Debug.Trace

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

seqE :: Event s a -> Event s a
seqE e = newEvent $ SeqE e

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

seqB :: Behaviour s a -> Behaviour s a
seqB b = newBehaviour $ SeqB b

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
  Never     ::                                  EventTerm s a
  Always    :: a                             -> EventTerm s a
  FMap      :: (a -> b) -> Event s a         -> EventTerm s b 
  Join      :: Event s (Event s a)           -> EventTerm s a
  WatchRef       :: SIORef a                      -> EventTerm s a
  WatchBehaviour :: Behaviour s (Maybe a)    -> EventTerm s a -- watch it!
  PlannedNow     :: Event s (Now s a)             -> EventTerm s a -- watch it!
  Planned        :: Event s (Behaviour s a)  -> EventTerm s a 
  SeqE      :: Event s a                     -> EventTerm s a
  SameAsE   :: Event s a                     -> EventTerm s a



newEventState :: EventTerm s a -> EventState s a 
newEventState t = ES minBound t

{-# NOINLINE newEvent #-}
newEvent t = unsafePerformIO $newEventIO t 
  

newEventIO t =  do r <- newIORef (newEventState t)
                   return (E r)

ejoin e = newEvent (Join e)

watchRef :: SIORef a -> Event s a
watchRef r = newEvent (WatchRef r)


getEv :: Event s a -> PIOM s (Maybe a)
getEv (E r) = 
  do tryUpdate r
     rv <- lift $ readIORef r
     return (getVal rv)
  where 
    getVal (Oc a) = Just a
    getVal _      = Nothing

    tryUpdate r = 
     do rv <- lift $ readIORef r
        case rv of
          Oc a    -> return ()
          ES tu e -> 
            do t <- curTime 
               if tu >= t 
               then return ()
               else do ev <- update t e
                       lift $ writeIORef r ev
                       lift $ flattenRefs r
    update :: TimeStamp -> EventTerm s a -> PIOM s (EventState s a)
    update t e = case e of
        Never      -> return (ES maxBound Never)
        Always a   -> printErr "Always" >> return (Oc a)
        FMap f ed  -> do v <- getEv ed
                         case v of
                          Just a  -> return (Oc (f a))
                          Nothing -> return (ES t e)
        Join ed    -> do v <- getEv ed
                         case v of
                          Just e@(E rd) -> do rn <- lift $ readIORef rd
                                              case rn of
                                                Oc a -> return (Oc a)
                                                _    -> return (ES t (SameAsE e))
                          Nothing -> return (ES t e)
        WatchRef w -> do v <- lift $ readSIORef w
                         case v of
                          Just a  -> return (Oc a)
                          Nothing -> return (ES t e)
        WatchBehaviour b -> do bv <- getBehaviour b
                               case bv of
                                 Just a  -> return (Oc a)
                                 Nothing -> return (ES t e)
        PlannedNow e2 -> do m <- getEv e2
                            case m of
                               Just (Now t) -> do a <- runPlanIO' t
                                                  return (Oc a)
                               Nothing -> return (ES t e)
        Planned e2 -> do m <- getEv e2
                         case m of
                          Just b -> do a <- getBehaviour b
                                       return (Oc a)
                          Nothing -> return (ES t e)
        SeqE e    -> do m <- getEv e
                        case m of
                         Just a -> return (Oc a)
                         Nothing -> addWeakWatch (WatchingE e) >> return (ES t (SameAsE e))
        SameAsE e -> do ev <- getEv e
                        case ev of
                          Just a -> return (Oc a)
                          _    -> return (ES t (SameAsE e))

    flattenRefs :: IORef (EventState s a) -> IO ()
    flattenRefs r = 
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
  SeqB     :: Behaviour s a                             -> BehaviourTerm s a
  SameAsB  :: Behaviour s a                             -> BehaviourTerm s a


data BehaviourState s a = 
    BS { lastUpdateB :: TimeStamp,
         termB       :: BehaviourTerm s a,
         curVal     :: a }
    | Constant a 


data Behaviour s a = B (IORef (BehaviourState s a))


newBehaviourState :: BehaviourTerm s a -> BehaviourState s a 
newBehaviourState t = BS minBound t undefined 

{-# NOINLINE newBehaviour #-}
newBehaviour t = unsafePerformIO $
   do r <- newIORef (newBehaviourState t)
      return (B r)

getBehaviour :: Behaviour s a -> PIOM s a
getBehaviour b =  getBehaviourAndConst b >>= return . fst

getBehaviourAndConst :: Behaviour s a -> PIOM s (a, Bool)
getBehaviourAndConst (B r) = 
  do tryUpdate r
     rv <- lift $ readIORef r
     case rv of
       BS _ _ a -> return (a, False)
       Constant a  -> return (a, True)
  where
    tryUpdate r = 
     do t <- curTime 
        s <- lift $ readIORef r
        case s of
         BS tu e _  | tu < t -> 
           do e' <- update t e
              lift $ writeIORef r e'
              lift $ flattenRefs r
         _ -> return ()

    update :: TimeStamp -> BehaviourTerm s a -> PIOM s (BehaviourState s a)
    update t e = case e of
        Hold   a   -> return (Constant a)
        BBind m f   -> do (mv,mc) <- getBehaviourAndConst m
                          let x = f mv
                          (xv,xc) <- getBehaviourAndConst x
                          case (mc, xc) of
                           (True, True) -> return (Constant xv)
                           (True,False) -> return (BS t (SameAsB x) xv)
                           (False,_)    -> return (BS t e xv)
        Switch b ev -> do evv <- getEv ev
                          case evv of
                           Just b' -> do bv <- getBehaviour b'
                                         return (BS t (SameAsB b') bv)
                           Nothing -> do v <- getBehaviour b
                                         return (BS t e v)
        Plan ev -> do getEv ev
                      evv <- lift $ newEventIO $ Planned ev
                      addWeakWatch (WatchingE ev)
                      return (BS t e evv)
        WhenJust b -> do getBehaviour b
                         ev <- lift $ newEventIO $ WatchBehaviour b
                         addWeakWatch (WatchingE ev)
                         return (BS t e ev)
        FixB f    ->  do v <- mfix (getBehaviour . f)
                         return (BS t (FixB f) v)
        SeqB b    -> do v <- getBehaviour b
                        addWeakWatch (WatchingB b)
                        return (BS t (SameAsB b) v)
        SameAsB b -> do bv <- getBehaviour b
                        return (BS t e bv)
    flattenRefs :: IORef (BehaviourState s a) -> IO ()
    flattenRefs r = 
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
    PrintState  :: Show a => Behaviour s a -> PlanIOPrim s ()
    PlanNow        :: Event s (Now s a)      -> PlanIOPrim s (Event s a)

runNow :: (forall s. Now s (Event s a)) -> IO a
runNow (Now p) = 
    do f <- newFlag
       evalStateT 
         (do e <- runPlanIO' p ;  runTimeSteps e)  
         (initState f)
  where initState f = PIOState (0) [] f
                 

runTimeSteps :: Event s a -> PIOM s a
runTimeSteps e = loop where
  loop = do incTime
            ev <- getEv e 
            case ev of
                Just a -> return a
                Nothing -> 
                  do runWatches
                     f <- getFlag 
                     lift $ waitForSignal f
                     loop
                 
runWatches :: PIOM s ()
runWatches = takeWatches >>= mapM_ tryRunWatch
    where tryRunWatch r@(WeakR w) = do m <- lift $ deRefWeak w
                                       case m of
                                        Just x -> runWatch x >>= reAdd r          
                                        Nothing -> return ()
          tryRunWatch r@(StrongR w) = runWatch w >>= reAdd r
          reAdd w True = addWatch w
          reAdd _ False = return ()
          runWatch :: Watching s -> PIOM s Bool
          runWatch (WatchingE e) = 
            do ev <- getEv e
               case ev of
                    Just n  -> return False
                    Nothing -> return True
          runWatch p@(WatchingB e) = getBehaviour e >> return True
              




runPlanIO' :: TermM (PlanIOPrim s) a-> PIOM s a
runPlanIO' t = case viewTermM t of
    Return a -> return a
    p :>>= f -> handlePrim p >>= runPlanIO' . f 
    where handlePrim :: PlanIOPrim s a -> PIOM s a
          handlePrim (LiftB b) = getBehaviour  b
          handlePrim (Fix f)   = mfix (runPlanIO' . getNow . f)
          handlePrim (PrintState b) = getBehaviour b >> (lift $ printStateBB b)
          handlePrim (Act m)   = startIO m
          handlePrim (PlanNow e)  = plan' e

plan' :: Event s (Now s a) -> PIOM s (Event s a)
plan' e = do let e' = newEvent (PlannedNow e)
             getEv e'
             addStrongWatch (WatchingE e')
             return e'



data Ref a = StrongR a
           | WeakR (Weak a) 


data Watching s = forall a. WatchingE (Event s a)
                | forall a. WatchingB (Behaviour s a)

data PIOState s = PIOState {     curTime' :: TimeStamp, -- write
                                 watches' :: [Ref (Watching s)], -- write
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

takeWatches :: PIOM s [Ref (Watching s)]
takeWatches =
 do b <- get
    put (b{watches' = []})
    return (watches' b)

addWatch :: Ref (Watching s) -> PIOM s ()
addWatch w = 
  do b <- get 
     put b { watches' = w : watches' b }

addStrongWatch :: Watching s -> PIOM s ()
addStrongWatch w = addWatch (StrongR w)

addWeakWatch :: Watching s -> PIOM s ()
addWeakWatch w = 
  do ww <- lift $ mkWeak w w Nothing
     addWatch (WeakR ww)


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
showBB (Constant x)  = putStr $ "Const" ++ (show x)

showBS :: BehaviourState s a -> IO ()
showBS (BS u t _) = do --putStr "(" 
                       putStr (show u) 
                       --putStr "->"
                       showB t
                       --putStr ")"
showBS (Constant _)  = putStr "Const"

printStateBB :: Show a => Behaviour s a -> IO ()
printStateBB (B r) = do v <- readIORef r
                        showBB v

printStateB ::  Behaviour s a -> IO ()
printStateB (B r) = do v <- readIORef r
                       showBS v

