{-# LANGUAGE ConstraintKinds, TypeFamilies,MultiParamTypeClasses, FlexibleInstances, TypeOperators,LambdaCase,UndecidableInstances,FunctionalDependencies #-}

module EventM where
import Control.Monad
import Control.Applicative
import FRPClasses 


type Time = Double

data TimeBounds = MinBound | T Time | MaxBound deriving (Ord,Eq)

type EventState m a = Either (EventM m a) (TimeBounds,a)

data EventM m a = E { lowerbound :: TimeBounds, try :: m (EventState m a) }

tryEv m = runEvent m >>= return . \case
      Left x -> Left x
      Right (_,a) -> Right a

makeEv :: Monad m => TimeBounds -> m (EventState m a) -> EventM m a
makeEv MaxBound _ = never -- throw data away
makeEv t m        = E t m

runEvent :: Monad m => EventM m a -> m (EventState m a)
runEvent (E lb m) = m >>= return . \case
      Left m      -> Left  (delay lb m)
      Right (t,a) -> Right (max lb t, a) 

rerunEvent :: Monad m => EventState m a -> m (EventState m a)
rerunEvent (Right x) = return $ Right x
rerunEvent (Left m)  = runEvent m

delay :: Monad m => TimeBounds -> EventM m a -> EventM m a
delay t (E lb m) = makeEv (max lb t) m

instance Monad m => Monad (EventM m) where
  return x = E MinBound (return $ Right (MinBound,x))
  m >>= f =  E (lowerbound m) $ 
     runEvent m >>= \case
        Left  m'    -> return (Left $ m' >>= f)
        Right (t,x) -> runEvent $ delay t (f x) 


instance Monad m => MonadPlus (EventM m) where
  mzero = E MaxBound (return $ Left mzero)
  mplus l r = makeEv (min (lowerbound l) (lowerbound r)) $
    do ls <- runEvent l
       rs <- runEvent r 
       return $ case (ls,rs) of
         (Right (tl,lv) , Right (tr, rv)) 
                | tr <= tl  -> Right (tr,rv)
                | otherwise -> Right (tl,lv)
         (Right (tl,lv), _) -> Right (tl,lv)
         (_, Right (tr,rv)) -> Right (tr,rv)
         (Left l', Left r') -> Left (mplus l' r')


instance Monad m => Functor (EventM m) where fmap = liftM
instance Monad m => Applicative (EventM m) where pure = return ; (<*>) = ap


