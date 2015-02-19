{-# LANGUAGE DeriveFunctor,TupleSections,TypeOperators,MultiParamTypeClasses, FlexibleInstances,TypeSynonymInstances, LambdaCase, ExistentialQuantification, Rank2Types, GeneralizedNewtypeDeriving #-}
module Impl.TimeEnv(Behavior, Event, Now, whenJust, switch, async, runFRP) where

import Control.Monad.Writer hiding (mapM_)
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class  hiding (mapM_)
import Control.Applicative hiding (empty)
import Control.Concurrent
import Data.IORef
import Data.Sequence
import Data.Foldable
import Data.Maybe
import System.IO.Unsafe -- only for unsafeMemoAgain at the bottom

import Prelude hiding (mapM_)

import Swap
import Impl.Ref
import Impl.EventBehavior
import Impl.UnscopedTIVar
import Impl.ConcFlag

-- Hide implementation details with newtypes

type Event    = E RealTimeEnv  
type Behavior = B RealTimeEnv  


data APlan = forall a. APlan (Ref (Event a))
type Plans = Seq APlan



newtype RealTimeEnv a = TE ( ReaderT (Flag,Clock,Round) (WriterT Plans IO) a)
 deriving (Monad,Applicative,Functor,MonadWriter Plans ,MonadReader (Flag,Clock,Round), MonadIO)


-- Plan stuff

instance TimeEnv RealTimeEnv where
 planM = planRef makeWeakRef 
 again = unsafeMemoAgain

planRef makeRef e   = runEvent e >>= \case
  Never -> return Never
  Occ m -> return <$> m
  e'    -> do r <- liftIO $ newIORef (Left e')
              let res = tryAgain r
              ref <- liftIO $ makeRef res
              tell (singleton (APlan ref))
              return res


tryAgain :: IORef (Either (Event (RealTimeEnv a)) a) -> Event a
tryAgain r = E $ liftIO (readIORef r) >>= \case 
  Right x -> trace "returned!" $ return (Occ x)
  Left e -> runEvent e >>= \case
    Never -> return Never
    Occ m -> do res <- m
                liftIO $ writeIORef r (Right res)
                return (Occ res)
    e'    -> do liftIO $ writeIORef r (Left e')
                return (tryAgain r)



-- Start IO Stuff 

newtype SpaceTime a = ST a 

instance Monad SpaceTime where
 return = ST
 (ST x) >>= f = f x

instance Functor SpaceTime where fmap = liftM
instance Applicative SpaceTime where pure = return ; (<*>) = ap

type Now = (Behavior :. SpaceTime)

instance Swap Behavior SpaceTime where
  swap (ST x) = ST <$> x 

async :: IO a -> Now (Event a)
async m = Close $ B $
       do (flag, clock,_) <- ask
          e <- liftIO $ schedule clock flag m
          return (ST e, never)

schedule :: Clock -> Flag -> IO a -> IO (Event a)
schedule clock flag  m = 
  do ti <-  newTIVar clock
     forkIO $ m >>= writeTIVar ti >> signal flag 
     return (tiVarToEv ti)

tiVarToEv :: TIVar a -> Event a
tiVarToEv ti = E $ 
  do (_,_,round) <- ask
     case ti `observeAt` round of
      Just a -> return (Occ a)
      Nothing -> return $ tiVarToEv ti

sampleB :: Now a -> RealTimeEnv a
sampleB m = do (ST x, _) <- runB (open m)
               return x

instance Swap Now Event where
 swap e = Close $ B $ 
    do e' <- planRef makeStrongRef (sampleB <$> e) 
       return (ST e', never)
                
-- Start main loop
data SomeEvent = forall a. SomeEvent (Event a)

tryPlan :: APlan -> SomeEvent -> RealTimeEnv ()
tryPlan p (SomeEvent e) = runEvent e >>= \case
             Occ  _  -> return ()
             Never   -> return ()
             E _     -> tell (singleton p)


makeStrongRefs :: Plans -> RealTimeEnv [(APlan, SomeEvent)] 
makeStrongRefs pl = catMaybes <$> mapM makeStrongRef (toList pl) where
 makeStrongRef :: APlan -> RealTimeEnv (Maybe (APlan, SomeEvent))
 makeStrongRef (APlan r) = liftIO (deRef r) >>= return . \case
         Just e  -> Just (APlan r, SomeEvent e)
         Nothing -> Nothing

runRound :: Event a -> Plans -> RealTimeEnv (Maybe a)
runRound e pl = 
  do pl' <- makeStrongRefs pl 
     mapM_ (uncurry tryPlan) pl'
     runEvent e >>= return . \case
       Occ x -> Just x
       _     -> Nothing

runTimeEnv :: Flag -> Clock -> RealTimeEnv a -> IO (a, Plans)
runTimeEnv f c (TE m) = 
 do r <- curRound c
    runWriterT (runReaderT m (f,c,r))


runFRP :: Now (Event a) -> IO a 
runFRP m = do f <- newFlag 
              c <- newClock             
              (ev,pl) <- runTimeEnv f c (sampleB m)
              loop f c ev pl where
  loop f c ev pl = 
     do waitForSignal f
        endRound c
        (done, pl') <- runTimeEnv f c (runRound ev pl)  
        case done of
          Just x  -> return x
          Nothing -> loop f c ev pl'
              

-- Memo stuff:

unsafeMemoAgain :: (x -> RealTimeEnv x) -> RealTimeEnv x -> RealTimeEnv x
unsafeMemoAgain again m = unsafePerformIO $ runMemo <$> newIORef (Nothing, m) where
   runMemo mem = 
    do (_,_,r) <- ask
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
