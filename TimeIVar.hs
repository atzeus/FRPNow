{-# LANGUAGE TupleSections, GeneralizedNewtypeDeriving, Rank2Types,RecursiveDo #-}

module TimeIVar

import Future


data TimeIVar a = TimeIVar FutureTime (IVar a)

newTimeIVar :: IO TimeIVar
newTimeIVar = return TimeIVar `ap` newFuture `ap` newIVar

writeTimeIVar TimeIVar a -> a -> IO ()
writeTimeIVar (TimeIVar f r) a = writeIVar r a >> futureIsNow f

getTimeIVarAt :: TimeIVar a -> PastTime -> Maybe (PastTime, a)
getTimeIVarAt (TimeIVar f r) t = case futureInfoAt f t of
    Just x  -> unsafePerformIO $ liftM fromJust (readIVar r)
    Nothing -> Nothing
