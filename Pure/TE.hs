{-# LANGUAGE MultiParamTypeClasses,TypeFamilies,ExistentialQuantification, Rank2Types, LambdaCase,GeneralizedNewtypeDeriving #-}
module TimeEnv where
import Control.Monad.Writer hiding (mapM_)
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Monad hiding (mapM_)
import Control.Applicative hiding (empty)
import Control.Monad.ST
import Control.Monad.ST.Lazy hiding (ST,runST)
import qualified Control.Monad.ST.Lazy as LST
import Data.STRef
import Data.Sequence
import Data.Foldable
import Prelude hiding (mapM_)
import EventM
import BehaviorM

type PlanState s a = Either (EventM (TimeEnv s) (TimeEnv s a)) (TimeBounds, a)
data APlan s = forall a. APlan (STRef s (PlanState s a))

type Plans s = Seq (APlan s)

data NextTime = NT Time | Inf

instance Monoid NextTime where
  mempty = Inf
  mappend Inf r = r
  mappend l Inf = l
  mappend (NT l) (NT r) = NT (min l r)


newtype TimeEnv a = TE ( WriterT Plans (ReaderT Round IO) a) 
 deriving (Monad,Applicative,Functor,MonadWriter Plans ,MonadReader Round)

runTimeEnv :: TimeBounds -> TimeEnv s a -> ST s (a, (NextTime,Plans s))
runTimeEnv t (TE m) = runReaderT (runWriterT m) t

liftST :: ST s a -> TimeEnv s a
liftST = TE . lift . lift

addPlan :: APlan s -> TimeEnv s ()
addPlan r = tell (Inf, singleton r)

setNextTime :: Time -> TimeEnv s ()
setNextTime t = 
    do n <- ask
       if T t > n 
       then tell (NT t, empty)
       else return ()


instance Plan (TimeEnv s) EventM where
  plan e = try e >>= \case
      Right (t,m) -> delay t . return <$> m
      Left e -> do r <- liftST $ newSTRef (Left e)
                   addPlan (APlan r)
                   return (E (lowerbound e) (tryAgain r))

tryAgain :: STRef s (PlanState s a) -> TimeEnv s (EventState (TimeEnv s) a)
tryAgain r = liftST (readSTRef r) >>= \case
  Right x -> return (Right x)
  Left e  -> runEvent e >>= \case
      Left e' -> do liftST $ writeSTRef r (Left e')
                    return (Left $ E (lowerbound e') (tryAgain r))
      Right (t,x) -> do res <- x
                        liftST $ writeSTRef r (Right (t,res))
                        return (Right (t,res))


tryPlans :: Plans s -> TimeEnv s ()
tryPlans p = mapM_ tryPlan p where          
  tryPlan (APlan r) = tryAgain r >>= \case
             Right _ ->  return ()
             Left _  -> addPlan (APlan r)

type EvRes a = (Time ,a)
data BRes a = BNF a (EvRes (BRes a))
            | ConstB a


primEvent :: Time -> a -> EventM (TimeEnv s) a
primEvent t a = E (T t) $ 
   do n <- ask
      if T t > n 
       then setNextTime t >> return (Left (primEvent t a))
       else return $ Right (T t,a)



toRes :: BehaviorM (TimeEnv s) a -> ST s (BRes a)
toRes (B m)  = lazyToStrictST $ loop MinBound empty where
  loop t l = do (hd, (nt, l')) <- strictToLazyST $ runTimeEnv t (step l) 
                case nt of
                 Inf -> return $ ConstB hd
                 NT t' -> do tl <- loop (T t') l'
                             return (BNF hd (t', tl))
  step l = 
    do p <- tryPlans l
       (h,_) <- m
       return h 


runBehavior :: (forall s. BehaviorM (TimeEnv s) a) -> BRes a
runBehavior b = runST $  toRes b



