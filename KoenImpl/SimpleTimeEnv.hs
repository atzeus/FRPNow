{-# LANGUAGE ExistentialQuantification, Rank2Types, GeneralizedNewtypeDeriving #-}
module SimpleTimeEnv where

import Control.Monad.ST
import Control.Monad.ST.Lazy hiding (ST,runST)
import qualified Control.Monad.ST.Lazy as LazyST
import Control.Monad.Writer hiding (mapM_)
import Control.Monad.Writer.Class
import Control.Monad.Reader.Class
import Control.Monad.Reader hiding (mapM_)
import Control.Monad hiding (mapM_)
import Control.Applicative hiding (empty)
import Data.STRef
import Data.Sequence
import Data.Foldable
import Prelude hiding (mapM_)

import EventBehavior

type Time = Integer

type PlanState s a = Either (Event (TimeEnv s) (TimeEnv s a)) a
data APlan s = forall a. APlan (STRef s (PlanState s a))
type Plans s = Seq (APlan s)


newtype TimeEnv s a = TE ( WriterT (Plans s) (ReaderT Time (LazyST.ST s)) a)
 deriving (Monad,Applicative,Functor,MonadWriter (Plans s) ,MonadReader Time)


makeEvent :: Time -> a -> Event (TimeEnv s) a
makeEvent t x = E $ do now <- ask
                       return $ 
                         if now >= t
                         then Right x
                         else Left (makeEvent t x)
                 

runBehavior :: (forall s. Behavior (TimeEnv s) a) -> [a]
runBehavior b = LazyST.runST $ toRes b

toRes :: Behavior (TimeEnv s) a -> LazyST.ST s ([a])
toRes (B m)  = loop 0 empty where
  -- loop :: Time -> Plans s -> LazyST.ST s [a]
  loop t l = do (hd, l') <-  runTimeEnv t (step l) 
                tl       <- loop  (t + 1) l'
                return (hd : tl)
  step l = do p <- tryPlans l
              (h,_) <- m
              return h 

runTimeEnv :: Time -> TimeEnv s a -> LazyST.ST s (a, Plans s)
runTimeEnv t (TE m) = runReaderT (runWriterT m) t

liftST :: ST s a -> TimeEnv s a
liftST = TE . lift . lift . strictToLazyST 

instance Plan (TimeEnv s) where
  plan e = runEvent e >>= \z -> case z of 
      Right m -> return <$> m
      Left e -> do r <- liftST $ newSTRef (Left e)
                   tell (singleton (APlan r))
                   return (tryAgain r)

tryAgain :: STRef s (PlanState s a) -> Event (TimeEnv s) a
tryAgain r = E $ liftST (readSTRef r) >>= \z -> case z of
  Right x -> return (Right x)
  Left e  -> runEvent e >>= \z -> case z of
      Left e' -> do liftST $ writeSTRef r (Left e')
                    return (Left $ tryAgain r)
      Right x -> do res <- x
                    liftST $ writeSTRef r (Right res)
                    return (Right res)

tryPlans :: Plans s -> TimeEnv s ()
tryPlans p = mapM_ tryPlan p where          
  tryPlan (APlan r) = runEvent (tryAgain r) >>= \z -> case z of
             Right _ -> return ()
             Left _  -> tell (singleton (APlan r))


