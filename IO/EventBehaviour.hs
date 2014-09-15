
{-# LANGUAGE GADTs, TupleSections #-}

module IO.EventBehaviour (TimeStamp,Event, never, watchRef, getEv, Behaviour, switch, getBehaviour, watchBehaviour ) where

import System.IO.Unsafe
import Data.IORef 
import Data.Int
import Control.Concurrent
import Control.Applicative 
import IO.ConcFlag
import IO.SingleWriteIORef
import Control.Monad
import Data.Maybe
import Race

type TimeStamp = Int64 -- this is a big place!

data Event s a = E (IORef (EventState s a)) 

data EventState s a 
    = ES { lastUpdateE :: TimeStamp,
           termE       :: EventTerm s a }
    | Oc a

data EventTerm s a where
  Never     ::                                  EventTerm s a
  Always    :: a                             -> EventTerm s a
  FMap      :: (a -> b) -> Event s a         -> EventTerm s b 
  Join      :: Event s (Event s a)           -> EventTerm s a
  WatchRef  :: SIORef a                      -> EventTerm s a
  WatchBehaviour :: Behaviour s (Maybe a)    -> EventTerm s a
  SameAsE   :: Event s a                     -> EventTerm s a
newEventState :: EventTerm s a -> EventState s a 
newEventState t = ES minBound t

{-# NOINLINE newEvent #-}
newEvent t = unsafePerformIO $
   do r <- newIORef (newEventState t)
      return (E r)

instance Functor (Event s)  where
  fmap f e = newEvent (FMap f e)

instance Monad (Event s) where
  return = always
  m >>= f = ejoin $ fmap f m

never :: Event s a
never = newEvent Never
always a = newEvent $ Always a

ejoin :: Event s (Event s a) -> Event s a
ejoin m = newEvent (Join m)

watchRef :: SIORef a -> Event s a
watchRef r = newEvent (WatchRef r)

watchBehaviour b = newEvent (WatchBehaviour b)



getEv :: TimeStamp -> Event s a -> IO (Maybe a)
getEv t (E r) = 
  do tryUpdate r
     rv <- readIORef r
     return (getVal rv)
  where 
    getVal (Oc a) = Just a
    getVal _      = Nothing

    tryUpdate r = 
     do rv <- readIORef r
        case rv of
          Oc a    -> return ()
          ES tu e -> if tu < t 
                     then return ()
                     else do ev <- update e
                             writeIORef r ev
                             flattenRefs r
    update :: EventTerm s a -> IO (EventState s a)
    update e = case e of
        Never      -> return (ES maxBound Never)
        Always a   -> return (Oc a)
        FMap f ed  -> do v <- getEv t ed
                         case v of
                          Just a  -> return (Oc (f a))
                          Nothing -> return (ES t e)
        Join ed    -> do v <- getEv t ed
                         case v of
                          Just e@(E rd) -> do rn <- readIORef rd
                                              case rn of
                                                Oc a -> return (Oc a)
                                                _    -> return (ES t (SameAsE e))
                          Nothing -> return (ES t e)
        WatchRef w -> do v <- readSIORef w
                         case v of
                          Just a  -> return (Oc a)
                          Nothing -> return (ES t e)
        WatchBehaviour b -> do bv <- getBehaviour t b
                               case bv of
                                 Just a  -> return (Oc a)
                                 Nothing -> return (ES t e)
        SameAsE e -> do ev <- getEv t e
                        case ev of
                          Just a -> return (Oc a)
                          _    -> return (ES t (SameAsE e))

    flattenRefs :: IORef (EventState s a) -> IO ()
    flattenRefs r = 
      do rv <- readIORef r
         case rv of
           ES _ (SameAsE (E r2)) -> 
            do rv2 <- readIORef r2
               case rv2 of
                ES _ (SameAsE (E r3)) -> writeIORef r rv2 
                _ -> return ()
           _ -> return ()


---------- Behaviour ----------------------------


data BehaviourTerm s a where
  Hold   :: a -> BehaviourTerm s a
  Ap     :: Behaviour s (a -> b) -> Behaviour s a -> BehaviourTerm s b
  Switch :: Behaviour s a -> Event s (Behaviour s a) -> BehaviourTerm s a
  SameAsB :: Behaviour s a -> BehaviourTerm s a
   
data BehaviourState s a = 
    BS { lastUpdateB :: TimeStamp,
         termB       :: BehaviourTerm s a,
         curVal     :: a }
    | Constant a

data Behaviour s a = B (IORef (BehaviourState s a))

instance Functor (Behaviour s) where fmap = (<$>)

newBehaviourState :: BehaviourTerm s a -> BehaviourState s a 
newBehaviourState t = BS minBound t undefined 

{-# NOINLINE newBehaviour #-}
newBehaviour t = unsafePerformIO $
   do r <- newIORef (newBehaviourState t)
      return (B r)

instance Applicative (Behaviour s) where
  pure a  = newBehaviour $ Hold a
  f <*> x = newBehaviour $ Ap f x

switch :: Behaviour s a -> Event s (Behaviour s a) -> Behaviour s a
switch b e = newBehaviour $ Switch b e


getBehaviour :: TimeStamp -> Behaviour s a -> IO a
getBehaviour t b =  getBehaviourAndConst t b >>= return . fst

getBehaviourAndConst :: TimeStamp -> Behaviour s a -> IO (a, Bool)
getBehaviourAndConst t (B r) = 
  do tryUpdate r
     rv <- readIORef r
     case rv of
       BS _ _ a -> return (a, False)
       Constant a  -> return (a, True)
  where
    tryUpdate r = 
     do s <- readIORef r
        case s of
         BS tu e _  | tu < t -> 
           do e' <- update e
              writeIORef r e'
              flattenRefs r
         _ -> return ()

    update :: BehaviourTerm s a -> IO (BehaviourState s a)
    update e = case e of
        Hold   a   -> return (Constant a)
        Ap f x     -> do (fv,fc) <- getBehaviourAndConst t f
                         (xv,xc) <- getBehaviourAndConst t x
                         if fc && xc
                         then return (Constant (fv xv))
                         else return (BS t e (fv xv))
        Switch b ev -> do evv <- getEv t ev
                          case evv of
                           Just b -> do bv <- getBehaviour t b
                                        return (BS t (SameAsB b) bv)
                           Nothing -> do v <- getBehaviour t b
                                         return (BS t e v)
        SameAsB b -> do bv <- getBehaviour t b
                        return (BS t e bv)
    flattenRefs :: IORef (BehaviourState s a) -> IO ()
    flattenRefs r = 
      do rv <- readIORef r
         case rv of
           BS _ (SameAsB (B r2)) _ -> 
            do rv2 <- readIORef r2
               case rv2 of
                BS _ (SameAsB (B r3)) _ -> writeIORef r rv2 
                _ -> return ()
           _ -> return ()

   



