{-# LANGUAGE DeriveFunctor,TupleSections,TypeOperators,MultiParamTypeClasses, FlexibleInstances,TypeSynonymInstances, LambdaCase, ExistentialQuantification, Rank2Types, GeneralizedNewtypeDeriving #-}
module Impl.TimeEnv(Behavior, Event, Now, never,  whenJust, switch, async, runFRP, unsafeSyncIO) where

import Control.Monad.Writer hiding (mapM_)
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class  hiding (mapM_)
import Control.Applicative hiding (empty)
import Data.IORef
import Data.Sequence hiding (length)
import Data.Foldable
import Data.Maybe
import System.IO.Unsafe -- only for unsafeMemoAgain at the bottom

import Prelude hiding (mapM_)

import Swap
import Impl.Ref
import Impl.EventBehavior
import Impl.PrimEv

type Event     = E Env  
type Behavior  = B Env  

type Plans = Seq Plan

data Plan = forall a. Plan (Ref (Event a))
type Env = SpaceTime
newtype SpaceTime a = ST { runST :: ReaderT Clock (WriterT Plans IO) a }
  deriving (Monad,Applicative,Functor)

-- unexported helper functions

getRound :: Env Round
getRound = ST $ ask >>= liftIO . curRound

addPlan :: Plan -> Env ()
addPlan p = ST $ tell (singleton p)

stIO :: IO a -> Env a
stIO m = ST $ liftIO m

-- Plan stuff

instance TimeEnv Env  where
 planM = makePlanRef makeWeakRef 
 again = unsafeMemoAgain

makePlanRef :: (forall x. x -> IO (Ref x)) -> Event (Env a) -> Env (Event a)
makePlanRef makeRef e   = runEvent e >>= \case
  Never -> return Never
  Occ m -> return <$> m
  e'    -> do r <- stIO $ newIORef (Left e')
              let res = tryAgain r
              ref <- stIO $ makeRef res
              addPlan (Plan ref)
              return res


tryAgain :: IORef (Either (Event (Env a)) a) -> Event a
tryAgain r = E $ 
 do -- liftIO $ putStrLn "Trying" 
    stIO (readIORef r) >>= \case 
     Right x -> return (Occ x)
     Left e -> runEvent e >>= \case
       Never -> return Never
       Occ m -> do res <- m
                   stIO $ writeIORef r (Right res)
                   return (Occ res)
       e'    -> do stIO $ writeIORef r (Left e')
                   return (tryAgain r)


-- Start IO Stuff 

type Now = (Behavior :. SpaceTime) 

instance Monad Now where
  return = Close .  pure . pure
  m >>= f = joinNow (fmap f m)

joinNow :: Now (Now a) -> Now a
joinNow = Close . fmap join . fmap absorb . open . fmap open

absorb :: SpaceTime (Behavior a) -> SpaceTime a
absorb m = m >>= curB

   
async :: IO a -> Now (Event a)
async m = Close $ pure $ ST $ 
  do c <- ask
     pe <- liftIO $ spawn c m
     return $ fromMaybeM $ (pe `observeAt`) <$> getRound

runNow :: Now a -> Env a
runNow m = do s <- curB $ open m
              s

instance Swap Now Event where
 swap e = Close $ return $ makePlanRef makeStrongRef (runNow <$> e) 

                
-- Start main loop
data SomeEvent = forall a. SomeEvent (Event a)

tryPlan :: Plan -> SomeEvent -> Env  ()
tryPlan p (SomeEvent e) = runEvent e >>= \case
             Occ  _  -> return ()
             Never   -> return ()
             E _     -> addPlan p


makeStrongRefs :: Plans -> Env  [(Plan, SomeEvent)] 
makeStrongRefs pl = catMaybes <$> mapM makeStrongRef (toList pl) where
 makeStrongRef :: Plan -> Env (Maybe (Plan, SomeEvent))
 makeStrongRef (Plan r) = stIO (deRef r) >>= return . \case
         Just e  -> Just (Plan r, SomeEvent e)
         Nothing -> Nothing

tryPlans :: Plans -> Env ()
tryPlans pl = 
  do pl' <- makeStrongRefs pl 
     -- liftIO $ putStrLn ("nrplans " ++ show (length pl'))
     mapM_ (uncurry tryPlan) pl'

runEnv :: Clock -> Env a ->  IO (a,Plans)
runEnv c m = runWriterT $ runReaderT (runST m) c

runFRP :: Now (Event a) -> IO a 
runFRP m = do c <- newClock             
              (ev,pl) <- runEnv c (runNow m)
              loop c ev pl where
  loop c ev pl = fst <$> runEnv c (curE ev) >>= \case 
    Just x -> return x
    Nothing -> 
      do waitEndRound c
         ((), pl') <- runEnv c (tryPlans pl) 
         loop c ev pl'
        
              

-- occasionally handy for debugging

unsafeSyncIO :: IO a -> Now a
unsafeSyncIO m = Close $ pure $ ST $ liftIO m

-- Memo stuff:

unsafeMemoAgain :: (x -> Env  x) -> Env  x -> Env  x
unsafeMemoAgain again m = unsafePerformIO $ runMemo <$> newIORef (Nothing, m) where
   runMemo mem = 
    do r <- getRound
       (v,m) <- stIO $ readIORef mem 
       res <- case v of
         Just (p,val) -> 
           case compare p r of
            LT -> m
            EQ -> return val
            GT -> error "non monotonic sampling!!"
         Nothing -> m
       stIO $ writeIORef mem (Just (r,res), again res)
       return res
