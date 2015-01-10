{-# LANGUAGE LambdaCase #-}
module Control.FRPNowImpl.SOS where

import Control.Applicative
import Control.Monad

data Event m a = E (m (Either (Event m a) a))
               | Never

never :: Event m a
never = Never

instance Monad m => Monad (Event m) where
  return x = E $ return $ Right x
  Never >>= _ = Never
  (E m) >>= f = 
    E $ m >>= \case
      Left  m' -> return $ Left $ m' >>= f
      Right x  -> 
        case f x of
         Never -> return $ Left Never
         E fm -> fm

{- This is symmetric! 
  
 ( note that first :: Event a -> Event a -> Event a
    is _not_ symmetric, have to decide in case  
    of simultaneity )
-} 
minTime :: Monad m => Event m a -> Event m b -> Event m ()
minTime Never r = () <$ r
minTime l Never = () <$ l
minTime (E lm) (E rm) = E $ rm >>= \case
  Right _ -> return $ Right ()
  Left r' -> lm >>= \case  
       Right _ -> return $ Right ()
       Left l' -> return $ Left $ minTime l' r'

data Behavior m a = B { runBehavior :: m (a, Event m (Behavior m a)) }

instance Monad m => Monad (Behavior m) where
  return x = B $ return (x, never)
  m >>= f = B $ do (h,t) <- runBehavior m
                   runBehavior $ f h `switch` ((>>= f) <$> t)
                   
-- associative!
switchEv :: Monad m => Event m (Behavior m a) -> Event m (Behavior m a) -> Event m (Behavior m a)
switchEv Never r = r
switchEv l Never = l
switchEv l@(E lm) r@(E rm) = b <$ minTime l r where
 b = B $ rm >>= \case
   Right rb -> runBehavior rb
   Left r' -> lm >>= \case
     Right lb -> do (hl,tl) <- runBehavior lb
                    return (hl, switchEv tl r')
     Left l' -> error "Cannot happen"
     
switch :: Monad m=>  Behavior m a -> Event m (Behavior m a) -> Behavior m a
switch b Never = b
switch (B bm) se@(E em) = 
 B $ em >>= \case
  Right (B x) -> x
  Left e -> do (h,t) <- bm
               let t' = (`switch` e) <$> t
               return (h, switchEv t' e)

class Monad m => Plan m where
  plan :: Event m (m a) -> m (Event m a)

whenJust :: Plan m => Behavior m (Maybe a) -> Behavior m (Event m a)
whenJust = B . whenJustm where
  whenJustm b = 
    do (h, t) <- runBehavior b
       case h of
        Just x -> return (return x, whenJust <$> t)
        Nothing -> do en <- plan (whenJustm <$> t)
                      let h = en >>= fst
                      let t' = en >>= snd
                      return (h,t')

instance Monad m => Functor (Event m) where fmap = liftM
instance Monad m => Applicative (Event m) where pure = return ; (<*>) = ap

instance Monad m => Functor (Behavior m) where fmap = liftM
instance Monad m => Applicative (Behavior m) where pure = return ; (<*>) = ap


{-
data Time s = MinBound | Time (Round s) deriving (Ord,Eq)

data EvState s a = After (Time s) (Event s a) 
                 | Occurred (Time s) a

newtype Event s a = E { runEv' :: Time s -> EvState s a }

again :: EvState s a -> Event s a
again es = 
  let x = E $ \t ->
       case es of
         Occurred t' _ 
           | t' <= t   -> es
           | otherwise -> After t x
         After t' e 
           | t > t'    -> runEv' e t
           | otherwise -> After t x
  in x

runEv :: Event s a -> Time s -> Maybe (Time s,a)
runEv e t = case runEv' e t of
              Occurred t' a -> Just (t',a)
              After  _ _   -> Nothing

makeEvent :: (Round s -> Maybe (Round s, a)) -> Event s a
makeEvent f = 
  let x = E $ \t -> 
            case t of
              MinBound -> After MinBound x
              Time ts -> case f ts of
                  Just (t',a) -> Occurred (Time t') a
                  Nothing    -> After (Time ts) x
  in x

never :: Event s a
never = E $ const $ After MinBound never

instance Monad (Event s) where
  return x = E $ const (Occurred MinBound x)
  m >>= f  = memo $ bind m f where
   bind m f = E $ \t -> 
     case runEv' m t of
     After d m'   -> After d (bind m' f)
     Occurred t' x -> 
        case runEv' (f x) t of
           Occurred t'' y -> Occurred (max t' t'') y
           After    t'' e -> After (max t' t'') e

instance Functor (Event s) where
  fmap = liftM
instance Applicative (Event s) where
  pure = return
  (<*>) = ap

first :: Event s a -> Event s a -> Event s a
first l r =  memo $ first' l r where 
  first' l r = E $ \t -> 
    case runEv' r t of
     Occurred tr vr -> 
         case prev l t of
           Occurred tl vl -> if tl < tr then Occurred tl vl else Occurred tr vr
           _              ->  Occurred tr vr
     After dl r' -> 
       case runEv' l t of
           Occurred tl vl -> Occurred (max dl tl) vl
           After dr l'     -> After (max dl dr) (first' l' r')
  prev e t = case t of
         MinBound -> After MinBound e
         Time ts -> runEv' e (Time $ prevRound ts)

memo :: Event s a -> Event s a
memo e = E $ \t -> unsafePerformIO $ runMemo t where
  mvar = unsafePerformIO $ newMVar (After MinBound e)
  {-# NOINLINE mvar #-}  
  runMemo t = 
    do es <- takeMVar mvar 
       let es' = runEv' (again es) t
       putMVar mvar es'
       return es'

{-# NOINLINE memo #-}  
-}
