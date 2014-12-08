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

getEv :: Event a -> Now (Maybe a)
getEv (x :@ t) = hasPassed t >>= return . \case
                 True -> Just x
                 False -> Nothing

asyncIO :: IO a -> Now (Event a)
asyncIO m = 
  do v <- syncIO newIVar
     t <- startIO (m >>= writeIVar v)
     return (ivarVal v :@ t)

everyRoundEv :: Now (Maybe a) -> Now (Event a)
everyRoundEv m =  
 do v <- syncIO newIVar
    t <- everyRound (tryRun v)
    return (ivarVal v :@ t)
 where tryRun iv = m >>= \case
        Just x -> syncIO (writeIVar iv x) >> return True
        Nothing -> return False


{-
firstObs :: Event a -> Event a -> Now (Event a)
firstObs l r = everyRoundEv sample where
  sample = do lv <- getEv l
              rv <- getEv r
              case (lv,rv) of
               (_, Just r) -> return (Just r)
               (Just l, _) -> return (Just l)
               _           -> return Nothing

-}
planIO :: Event (Now a) -> Now (Event a)
planIO e = everyRoundEv tryRun where
  tryRun = getEv e >>= \case
       Just x -> Just <$> x
       Nothing -> return Nothing


                   
