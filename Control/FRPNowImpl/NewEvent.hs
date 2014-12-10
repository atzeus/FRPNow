{-# LANGUAGE LambdaCase #-}
module Control.FRPNowImpl.NewEvent where
import Control.FRPNowImpl.NowTime
import Control.FRPNowImpl.IVar
import Control.Monad
import Control.Applicative

data Event a = a :@ Time

instance Monad Event where
  return x = x :@ bigBang
  (a :@ t) >>= f = b :@ maxTime t t2 where b :@ t2 = f a

instance Functor Event where
  fmap f (a :@ t) = f a :@ t

instance Applicative Event where
  pure = return
  (<*>) = ap

never = undefined :@ pigsFly

getEv :: Event a -> Now (Maybe a)
getEv (x :@ t) = hasPassed t >>= return . \case
                 True -> Just x
                 False -> Nothing

asyncIO :: IO a -> Now (Event a)
asyncIO m = 
  do v <- syncIO newIVar
     t <- startIO (m >>= writeIVar v)
     return (ivarVal v :@ t)

firstObsNow :: Event a -> Event a -> Now (Event a)
firstObsNow l@(a :@ tl) r@(b :@ tr) = 
 do t <- earliestObs tl tr
    planIO (checkBoth :@ t)  where
  checkBoth = getEv r >>= \case
     Just x -> return x
     Nothing -> getEv l >>= \case
                    Just x -> return x

planIO :: Event (Now a) -> Now (Event a)
planIO (a :@ t) = 
 do v <- syncIO newIVar
    doAtTime t (a >>= syncIO . writeIVar v)
    return (ivarVal v :@ t)

runNow :: Now (Event a) -> IO a
runNow n = 
  do v <- newIVar
     startNowTime (ntime v)
     return (ivarVal v) where
 ntime v = 
      do (a :@ t) <- n
         doAtTime t (syncIO $ writeIVar v a)
         return t



                   
