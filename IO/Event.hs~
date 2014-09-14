
{-# LANGUAGE GADTs, TupleSections #-}

module IO.Event (TimeStamp,Event, never,always, getEv) where

import System.IO.Unsafe
import Data.IORef 
import Data.Int
import Control.Concurrent
import Control.Applicative 
import IO.ConcFlag
import Control.Monad
import Data.Maybe
import Race

type TimeStamp = Int64 -- this is a big place!

data Event s a where
  Never     :: Event s a
  Always    :: a -> Event s a
  FMap      :: (a -> b) -> IORef (TimeStamp, Maybe b) -> Event s a -> Event s b 
  Join      :: Event s (Event s a) -> Event s a
  WatchRef  :: IORef (TimeStamp, Maybe a) -> IORef (Maybe a)       -> Event s a

instance Functor (Event s)  where
  {-# NOINLINE fmap #-}
  fmap f Never = Never
  fmap f (Always x) = Always (f x)
  fmap f e          = unsafePerformIO $ (\r -> FMap f r e) <$> newIORef (minBound,Nothing) -- for sharing!

instance Monad (Event s) where
  return = always
  m >>= f = ejoin $ fmap f m

never :: Event s a
never = Never
always = Always
ejoin :: Event s (Event s a) -> Event s a
ejoin Never = Never
ejoin (Always a) = a
ejoin m = Join m

{-
startIO :: Flag -> IO a -> IO (Event s a)
startIO f a = do im <- newIORef (minBound,Nothing)
                 r <-  newIORef Nothing
                 forkIO $ do v <- a
                             writeIORef r (Just v)
                             signal f
                 return (IOEvent im r)

-}
getEv :: TimeStamp -> Event s a -> IO (Maybe a)
getEv t e = case e of
  Never         -> return Nothing
  Always a      -> return (Just a)
  FMap f r e    -> updateFMapEv f r e    >> readIORef r >>= return . snd
  WatchRef r vr -> updateIOEv r vr       >> readIORef r >>= return . snd
  Join e        -> do e1 <- getEv t e
                      case e1 of
                       Just e2 -> getEv t e2
                       Nothing -> return Nothing
 
  where updateIOEv :: IORef (TimeStamp,Maybe a) -> IORef (Maybe a) -> IO ()
        updateIOEv ir vr = 
           do (p,m) <- readIORef ir
              if isJust m || p == t
              then return ()
              else do vm <- readIORef vr
                      writeIORef ir (t, vm)
        updateFMapEv :: (a -> b) -> IORef (TimeStamp,Maybe b) -> Event s a -> IO ()
        updateFMapEv f r e = 
          do (p,m) <- readIORef r
             if isJust m || p == t
             then return ()
             else do vm <- getEv t e
                     writeIORef r (t, fmap f vm)

combineMaybe (Just a) (Just b) = Just (Tie a b)
combineMaybe (Just a) Nothing  = Just (L a)
combineMaybe Nothing (Just b)  = Just (R b)
combineMaybe Nothing Nothing = Nothing




