{-# LANGUAGE TypeFamilies,ExistentialQuantification, Rank2Types, LambdaCase,GeneralizedNewtypeDeriving #-}
import Control.Monad.Writer hiding (mapM_)
import Control.Monad.Writer.Class
import Control.Monad hiding (mapM_)
import SOSTT
import Control.Applicative hiding (empty)
import Data.Sequence
import Data.Foldable
import Prelude hiding (mapM_)

data APlan m = forall a. APlan (RefType m (Either (Event (PlanM m) (PlanM m a)) a))

type Plans m = Seq (APlan (PlanM m))

newtype PlanM m a = PlanM ( WriterT (Plans m) m a ) deriving (Monad,Applicative,Functor,MonadWriter (Plans m))

runPlanM :: Monad m => PlanM m a -> m (a, Plans m)
runPlanM (PlanM m) = runWriterT m

class (Functor m, Applicative m, Monad m) => Memory m where
  type RefType m :: * -> *
  newRef   :: a -> m (RefType m a)
  readRef  :: RefType m a -> m a
  writeRef :: RefType m a -> a -> m ()

instance Memory m => Memory (PlanM m) where
  type RefType (PlanM m) = RefType m
  newRef   x   = PlanM $ lift $ newRef   x
  readRef  x   = PlanM $ lift $ readRef  x
  writeRef x y = PlanM $ lift $ writeRef x y

instance Monad m => MemoTime (PlanM m)

instance Memory m => Plan (PlanM m) where
  plan Never = return Never
  plan (E m) = m >>= \case
    Left e  -> do r <- newRef (Left e)
                  schedule r
                  return (toEv $ tryAgain r)
    Right x -> return <$> x

schedule :: RefType m (Either (Event (PlanM m) (PlanM m a)) a) -> PlanM m ()
schedule r = PlanM $ tell (singleton (APlan r))

tryAgain :: Memory m => RefType m (Either (Event (PlanM m) (PlanM m a)) a) -> PlanM m (Maybe a)
tryAgain r = E $ readRef r >>= \case
  Right x -> return (Right x)
  Left e  -> runEvent e >>= \case
      Left e' -> do writeRef r (Left e')
                    return Nothing
      Right x -> do res <- x
                    writeRef r (Right res)
                    return (Just res)

tryPlans :: Memory m => Plans m -> PlanM m ()
tryPlans p = mapM_ tryPlan p where          
  tryPlan (APlan r) = tryAgain r >>= \case
             Just _ ->  return ()
             Nothing -> schedule r

{-
runBehavior :: Enum t => (forall s. Behavior (TE t s) a) -> [a]
runBehavior b = LST.runST $ LST.strictToLazyST $ sampleAll b

sampleAll :: Enum t => Behavior (TE t s) a -> ST s [a]
sampleAll (B m)  = loop empty [toEnum 0..] where
  loop l (ht:tt) = 
    do p <- tryPlans l ht
       ((h,_),pb) <- runTimeEnv m ht
       t     <- loop (p >< pb) tt
       return (h : t) 




runTimeEnv :: TE t s a -> t -> ST s (a,Plans t s)
runTimeEnv (TE m) t = runWriterT (runReaderT m t)
-} 
