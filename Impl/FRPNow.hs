{-# LANGUAGE  Rank2Types,OverlappingInstances, DeriveFunctor,TupleSections,TypeOperators,MultiParamTypeClasses, FlexibleInstances,TypeSynonymInstances, LambdaCase, ExistentialQuantification, GeneralizedNewtypeDeriving #-}
module Impl.FRPNow(Behavior, Event, Now, never, whenJust, switch, sample, async, runNow, unsafeSyncIO) where

import Control.Monad.Writer hiding (mapM_)
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class  hiding (mapM_)
import Control.Applicative hiding (Const,empty)
import Data.IORef
import Data.Sequence hiding (length,reverse)
import Data.Foldable
import Data.Maybe
import System.IO.Unsafe -- only for unsafeMemoAgain at the bottom
import Debug.Trace
import Prelude hiding (mapM_)

import Swap
import Impl.Ref
import Impl.PrimEv

-- comment/uncomment here to disable optimization
again :: (x -> M  x) -> M x -> M x
again = unsafeMemoAgain
--again f m = m 


type N = M


-- Start events, a bit more optimized than in paper


data Event a = E (M (Event a))
             | Occ a
             | Never

runE :: Event a -> M (Event a)
runE (Occ a) = return (Occ a)
runE Never   = return Never
runE (E m)   = m

curE ::  Event a -> M (Maybe a)
curE e = runE e >>= return . \case
  Occ x -> Just x
  _     -> Nothing


fromMaybeM :: M (Maybe a) -> Event a
fromMaybeM m =
  let x = E $ m >>= return . \case
           Just x -> Occ x
           _      -> x
  in x
   
never :: Event a
never = Never

instance Monad Event where
  return = Occ 
  Never    >>= f = Never
  (Occ x)  >>= f = f x
  ev    >>= f = memoE $
    runE ev >>= \case
      Never ->  return Never
      Occ x ->  runE (f x)
      e'    ->  return (ev >>= f)

rerunE = runE

memoE :: M (Event a) -> Event a
memoE m = E (again rerunE m)

instance Functor Event where
  fmap = liftM

instance Applicative Event where
  pure = return
  (<*>) = ap

-- Start behaviors, also a bit more optimized than in paper

data Behavior a = B (M  (a, Event (Behavior a)))
                | Const a

runB :: Behavior a -> M  (a, Event (Behavior a))
runB (Const x) = return (x, never)
runB (B m)     = m

curB :: Behavior a -> M a
curB b = fst <$> runB b

instance Monad Behavior where
  return = Const
  (Const x) >>= f = f x 
  b     >>= f = memoB $
    do (h,t) <- runB b
       (fh,th) <- runB (f h)
       return (fh, switchEv th ((b >>= f) <$ t))
            
switch :: Behavior a -> Event (Behavior a) -> Behavior a
switch b Never   = b
switch _ (Occ b) = b
switch b e   = memoB $ runE e >>= \case
   Occ   x -> runB x                 
   Never   -> runB b
   _       -> do (h,t) <- runB b
                 return (h, switchEv t e)

switchEv :: Event (Behavior a) -> Event (Behavior a) -> Event (Behavior a)
switchEv l Never     = l
switchEv l (Occ r)   = Occ r
switchEv Never r     = r
switchEv (Occ x) r   = Occ (x `switch` r)
switchEv (E l) (E r) = E $ 
  r >>= \case
    Occ y -> return $ Occ y
    r' -> l >>= return . \case 
           Occ x -> Occ (x `switch` r')
           l'    -> switchEv l' r'



whenJust :: Behavior (Maybe a) -> Behavior (Event a)
whenJust (Const Nothing)  = pure never
whenJust (Const (Just x)) = pure (pure x)
whenJust b = memoB $ 
  do (h, t) <- runB b
     case h of
      Just x -> return (return x, whenJust b <$ t)
      Nothing -> do en <- planM (runB (whenJust b) <$ t)
                    return (en >>= fst, en >>= snd)

rerunB :: (a, Event (Behavior a)) -> M (a, Event (Behavior a))
rerunB (h,t) = runE t >>= \case
      Occ x -> runB x
      Never     -> return (h,Never)
      t'        -> return (h,t')

memoB :: M (a, Event (Behavior a)) -> Behavior a
memoB m = B (again rerunB m)


instance Swap Behavior Event  where 
   swap Never = pure Never
   swap (Occ x) = Occ <$> x
   swap e       = B $
       runE e >>= \case
         Never -> return (Never, Never)
         Occ x -> runB (Occ <$> x)
         _    -> do ev <- planM (runB <$> e)  
                    return (fst <$> ev, (Occ <$>) <$> (ev >>= snd))

instance Functor Behavior where
  fmap = liftM

instance Applicative Behavior where
  pure = return
  (<*>) = ap



-- Memo stuff:

unsafeMemoAgain :: (x -> M  x) -> M x -> M x
unsafeMemoAgain again m = unsafePerformIO $ runMemo <$> newIORef (Nothing, m) where
   runMemo mem = 
    do r <- getRound
       (v,m) <- liftIO $ readIORef mem 
       res <- case v of
         Just (p,val) -> 
           case compare p r of
            LT -> m
            EQ -> return val
            GT -> error "non monotonic sampling!!"
         Nothing -> m
       liftIO $ writeIORef mem (Just (r,res), again res)
       return res


-- unexported helper functions

getRound :: N Round
getRound = ask >>= liftIO . curRound

addPlan :: Plan -> N ()
addPlan p = tell (singleton p)


-- Start main loop

type Plans = Seq Plan
type PlanState a = IORef (Either (Event (N a)) a)
data Plan = forall a. Plan (Ref (PlanState a))

type M = WriterT Plans (ReaderT Clock IO)

data SomePlanState = forall a. SomePlanState (PlanState a)

tryPlan :: Plan -> SomePlanState -> N  ()
tryPlan p (SomePlanState r) = tryAgain r >>= \case
             Occ  _  -> return ()
             Never   -> return ()
             E _     -> addPlan p


makeStrongRefs :: Plans -> N   [(Plan, SomePlanState)] 
makeStrongRefs pl = catMaybes <$> mapM makeStrongRef (toList pl) where
 makeStrongRef :: Plan -> N  (Maybe (Plan, SomePlanState))
 makeStrongRef (Plan r) = liftIO (deRef r) >>= return . \case
         Just e  -> Just (Plan r, SomePlanState e)
         Nothing -> Nothing

tryPlans :: Plans -> N  ()
tryPlans pl = 
  do pl' <- makeStrongRefs pl 
     -- stIO $ putStrLn ("nrplans " ++ show (length pl'))
     mapM_ (uncurry tryPlan) pl'

runN :: Clock -> N a ->  IO (a,Plans)
runN c m = runReaderT (runWriterT m) c

runNow :: Now (Event a) -> IO a 
runNow m = newClock >>= runReaderT start where
  start = do  (ev,pl) <- runWriterT (toN m)
              loop ev pl
  loop :: Event a -> Plans -> ReaderT Clock IO a
  loop ev pli =
   do  (er,ple) <- runWriterT (runE ev) 
       let pl = pli >< ple
       case er of
         Occ x   -> return x
         ev' -> 
           do  endRound
               ((), pl') <- runWriterT (tryPlans pl) 
               loop ev' pl' 
  endRound = ask >>= liftIO . waitEndRound 


-- Plan stuff

planM = makePlanRef makeWeakIORef



makePlanRef :: (forall a. IORef a -> IO (Ref (IORef a))) -> Event (N a) -> N (Event a)
makePlanRef makeRef e   = runE e >>= \case
  Never -> return Never
  Occ m -> return <$> m
  e'    -> do r <- liftIO $ newIORef (Left e')
              let res = E $ tryAgain r
              ref <- liftIO $ makeRef r
              addPlan (Plan ref)
              return res


tryAgain :: PlanState a -> N (Event a)
tryAgain r = 
   let x = do liftIO (readIORef r) >>= \case 
               Right x -> return (Occ x)  
               Left e -> runE e >>= \case
                 Never -> return Never
                 Occ m -> do res <- m
                             liftIO $ writeIORef r (Right res)
                             return (Occ res)
                 e'    -> do liftIO $ writeIORef r (Left e)
                             return (E x)
  in x
        


-- Start IO Stuff 

newtype Now a = Now {toN :: N a} deriving (Functor,Applicative,Monad)

sample :: Behavior a -> Now a
sample b = Now $ curB b
   
async :: IO a -> Now (Event a)
async m = Now $   
  do c <- ask
     pe <- liftIO $ spawn c m
     return $ fromMaybeM $ (pe `observeAt`) <$> getRound


instance Swap Now Event where
 swap e = Now $ planN (toN <$> e) 

planN :: Event (N a) -> N (Event a)
planN e = makePlanRef makeStrongRef e

-- occasionally handy for debugging

unsafeSyncIO :: IO a -> Now a
unsafeSyncIO m = Now $ liftIO m              
