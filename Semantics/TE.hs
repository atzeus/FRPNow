{-# LANGUAGE MultiParamTypeClasses,TypeSynonymInstances,TypeFamilies,ExistentialQuantification, Rank2Types, LambdaCase,GeneralizedNewtypeDeriving #-}
import Control.Monad.Writer hiding (mapM_)
import Control.Monad.Writer.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Monad.Reader.Class
import qualified Control.Monad.ST as SST
import Control.Monad.ST.Lazy
import Control.Monad hiding (mapM_)
import Data.STRef
import SOSTT
import Control.Applicative hiding (empty)
import Data.Sequence
import Data.Foldable
import Prelude hiding (mapM_)
import STMemoTime 

type Time = Double

data NextTime = NT Time | Inf

instance Monoid NextTime where
  mempty = Inf
  mappend Inf y = y
  mappend x Inf = x
  mappend (NT x) (NT y) = NT (min x y)

type PlanRef s a = STRef s (Either (Event (TimeEnv s) (TimeEnv s a)) a)

data APlan s = forall a. APlan (PlanRef s a)
type Plans s = Seq (APlan s)

newtype TimeEnv s a = TimeEnv ( WriterT (Plans s,NextTime) (ReaderT Time (ST s)) a) deriving (Monad,Applicative,Functor)

instance MonadReader Time (TimeEnv s) where
  ask = TimeEnv $ lift ask


tellNextTime :: Time -> TimeEnv s ()
tellNextTime t = 
  do n <- ask
     if t <= n
     then return ()
     else TimeEnv $ tell (mempty, NT t)

schedule :: PlanRef s a -> TimeEnv s ()
schedule r = TimeEnv $ tell (singleton (APlan r), mempty)

instance LiftST TimeEnv where
  liftST = TimeEnv . lift . lift . strictToLazyST

toEv :: Time -> a -> Event (TimeEnv s) a
toEv t a = maybeToEv $  
  do n <- ask
     tellNextTime t
     return $ if t >= n then Just a else Nothing

instance Plan (TimeEnv m) where
  plan Never = return Never
  plan (E m) = m >>= \case
    Left e  -> do r <- liftST (newSTRef (Left e))
                  schedule r
                  return (maybeToEv $ tryAgain r)
    Right x -> return <$> x

instance MemoTime (TimeEnv s) where
  memoTime = memoTimeST

tryAgain :: PlanRef s a -> TimeEnv s (Maybe a)
tryAgain r = liftST (readSTRef r) >>= \case
  Right x -> return (Just x)
  Left e  -> runEvent e >>= \case
      Left e' -> do liftST $ writeSTRef r (Left e')
                    return Nothing
      Right x -> do res <- x
                    liftST $ writeSTRef r (Right res)
                    return (Just res)

runTimeEnv :: Time -> TimeEnv s a -> ST s (a, (Plans s, NextTime))
runTimeEnv t (TimeEnv m) = runReaderT (runWriterT m) t

tryPlans :: Plans s -> TimeEnv s ()
tryPlans p = mapM_ tryPlan p where          
  tryPlan (APlan r) = tryAgain r >>= \case
             Just _ ->  return ()
             Nothing -> schedule r

sampleBehavior :: (forall s. Behavior (TimeEnv s) a) -> [(Time,a)]
sampleBehavior b = SST.runST $ lazyToStrictST $ sampleAll b

sampleAll :: Behavior (TimeEnv s) a -> ST s [(Time,a)]
sampleAll (B m) = loop empty 0.0 where
  loop l t = do (h, (l',nt)) <- runTimeEnv t (round l)
                case nt of
                 Inf   -> return [(t,h)]
                 NT nt -> do ts <- loop l' nt
                             return ((t,h):ts)
  round l = do p <- tryPlans l
               x <- fst <$> m
               return x

