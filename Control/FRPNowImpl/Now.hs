{-# LANGUAGE ScopedTypeVariables,TypeSynonymInstances,Rank2Types,TupleSections,LambdaCase,ExistentialQuantification,GADTs,GeneralizedNewtypeDeriving #-}
module Control.FRPNowImpl.Now(Event, never, minTime, Behavior, switch, whenJust, Now, syncIO, asyncIO, asyncOS, evNow, curIO, planIO, planIOWeak, planIOWeakKey, runFRPLocal, runFRP, Global) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.Reader
import Debug.Trace
import Data.Maybe
import Data.TIVar
import Data.IVar
import Control.Concurrent
import Control.Concurrent.ConcFlag
import System.IO.Unsafe
import Data.Ref
import Data.IORef


-- Events 


data Event s a = E { runEvent' :: Now s (Either (Event s a) a) }
               | Never

data EMemoOutcome s a = EMemoAgain (Now s (EMemoOutcome s a))
                      | ESameAs (Event s a)
                      | EDone a

memoE :: Now s (EMemoOutcome s a) -> Event s a
memoE m = E $ runMemo where
  mem = unsafePerformIO $ newIORef (Nothing, m)
  {-# NOINLINE mem #-}  
  runMemo = 
   do (r,m) <- syncIO $ readIORef mem 
      i <- getRound
      case r of
       Just (j,r) | i == j -> return r
       _ ->
        do c <- m
           (res,m') <- case c of
            EDone a       -> return (Right a         , return c          )
            EMemoAgain m' -> return (Left $ E runMemo, m'                )
            ESameAs Never -> return (Left Never      , return c          )
            ESameAs (E e) -> e >>= \case
                Left e' -> return (Left e'         , return (ESameAs e'))
                Right a -> return (Right a         , return (EDone a)   )
           syncIO $ writeIORef mem (Just (i,res), m')
           return res
{-# NOINLINE memoE #-}  

never :: Event s a
never = Never

instance Monad (Event s) where
  return x = E $ return $ Right x
  Never >>= f = Never
  (E m) >>= f = memoE $ bind m f where
    bind m f = m >>= return .  \case
        Left Never  -> ESameAs Never
        Left (E m') -> EMemoAgain (bind m' f)
        Right x     -> ESameAs (f x)
                    
{- This is symmetric! 
  
 ( note that first :: Event a -> Event a -> Event a
    is _not_ symmetric, have to decide in case  
    of simultaneity )
-} 
minTime :: Event s a -> Event s b -> Event s ()
minTime Never r = () <$ r
minTime l Never = () <$ l
minTime (E l) (E r) = memoE (min l r) where
  min l r = r >>= \case
       Right _     -> return $ EDone ()
       Left Never  -> return $ ESameAs $ () <$ (E l)
       Left (E r') -> l >>= return . \case
         Right _     -> EDone ()
         Left Never  -> ESameAs $ () <$ (E r')
         Left (E l') -> EMemoAgain (min l' r')

makeEvent :: ( Round s -> Maybe (Round s,a)) -> Event s a
makeEvent f = 
  let x = E $ do 
                 r <- getRound
                 case f r of
                  Just (_,a) -> return $ Right a
                  Nothing    -> return $ Left x
  in x

instance Functor (Event s) where fmap = liftM
instance Applicative (Event s) where pure = return ; (<*>) = ap


-- Behaviors


data Behavior s a = B { runBehavior ::  Now s (a, Event s (Behavior s a)) }

data BMemoOutcome s a = BMemoAgain a (Event s (Now s (BMemoOutcome s a)))
                      | BSameAs (Behavior s a)


memoB :: Now s (BMemoOutcome s a) -> Behavior s a
memoB m = B $ runMemo where
  mem = unsafePerformIO $ newIORef m
  {-# NOINLINE mem #-}  
  runMemo = 
    do m <- syncIO $ readIORef mem 
       syncIO $ writeIORef mem (error "<<FRP loop>>")
       c <- m
       (res,m') <- case c of
         BMemoAgain h t -> return ( (h, B runMemo <$ t), again h t)
         BSameAs b -> do (h,t) <- runBehavior b
                         return ((h,t), againSameAs b h t)
       syncIO $ writeIORef mem m'
       return res
  again h Never = return (BMemoAgain h Never)
  again h (E e) = e >>= \case
            Left e' -> return (BMemoAgain h e')
            Right x -> x
  againSameAs _ h Never = return (BMemoAgain h Never)
  againSameAs b h (E e) = e >>= \case
            Left _ -> return (BSameAs b)
            Right x -> return (BSameAs x)
{-# NOINLINE memoB #-}  

instance Monad (Behavior s) where
  return x = B $ return (x, never)
  m >>= f  = memoB $ bind m f where
    bind m f = 
       do (h ,t ) <- runBehavior m
          (fh,ft) <- runBehavior (f h)
          let e   = switchEv ft ((`bind` f) <$> t)
          return $ BMemoAgain fh e

                    
                   
-- associative! 
-- This means, denotationally, 
-- switchEv (tl,bl) (tr,br) 
--  | tr <= tl = (tr, br)
--  | otherwise = (tl, bl `switch` (tr,br))                              
switchEv :: Event s (Behavior s a) -> Event s (Now s (BMemoOutcome s a)) -> Event s (Now s (BMemoOutcome s a))
switchEv Never r = r
switchEv l Never = return . BSameAs <$> l
switchEv l@(E lm)  r@(E rm) = b <$ minTime l r where
 b = rm >>= \case
      Right x -> x
      Left r' -> lm >>= \case
          Right lb -> do (h,t) <- runBehavior lb
                         return $ BMemoAgain h (switchEv t r')
                 

switch :: Behavior s a -> Event s (Behavior s a) -> Behavior s a
switch b Never = b
switch b (E e) = memoB $ e >>= \case
  Right x  -> return $ BSameAs x
  Left  e -> do (h,t) <- runBehavior b
                let e' = return . BSameAs <$> e
                return $ BMemoAgain h (switchEv t e')


whenJust :: Behavior s (Maybe a) -> Behavior s (Event s a)
whenJust b = memoB (wj b) where
  wj b = liftM (\(h,t) -> BMemoAgain h t) (whenJustm b) where
    whenJustm b = 
     do (h, t) <- runBehavior b
        case h of
         Just x -> return (return x, wj <$> t)
         Nothing -> do en <- planIOWeak (whenJustm <$> t)
                       let h = en >>= fst 
                       let t' = en >>= snd
                       return (h, t')

curIO :: Behavior s a -> Now s a
curIO (B m) = fst <$> m 




instance Functor (Behavior s) where fmap = liftM
instance Applicative (Behavior s) where pure = return ; (<*>) = ap

-- New, simpler implementation of Now...



data Plan s = forall a. Plan (Event s a)

data SomeMVar = forall a. SomeMVar (MVar a)

data Env s = Env {
  clock :: Clock s,
  plans :: IORef [Ref (Plan s)],
  flag  :: Flag ,
  strongRefs :: IORef [SomeMVar] 
  -- keep MVars that are never written to alive to prevent indefinitly blocked on
  -- mvar error
 }

newtype Now s a = Now { runNow' ::  ReaderT (Env s) IO a } deriving (Functor, Applicative, Monad)

getEnv :: Now s (Env s)
getEnv = Now ask

getRound :: Now s (Round s)
getRound = do e <- getEnv 
              syncIO $ curRound (clock e)

syncIO :: IO a -> Now s a
syncIO m = Now $ liftIO m

asyncIO :: IO a -> Now s (Event s a)
asyncIO m =  
  do env <- getEnv
     ti <- syncIO $ newTIVar (clock env)
     syncIO $ forkIO $ m >>= writeTIVar ti >> signal (flag env)
     return $ makeEvent (observeAt ti)

asyncOS :: IO a -> Now s (Event s a)
asyncOS m =  
  do env <- getEnv
     ti <- syncIO $ newTIVar (clock env)
     syncIO $ forkOS $ m >>= writeTIVar ti >> signal (flag env)
     return $ makeEvent (observeAt ti)

evNow :: Event s a -> Now s (Maybe a)
evNow Never = return Nothing
evNow (E m) = m >>= return . \case
    Right x -> Just x
    Left _  -> Nothing


planIO :: Event s (Now s a) -> Now s (Event s a)
planIO  = planIO' (\_ y -> makeStrongRef y)

planIOWeak :: Event s (Now s a) -> Now s (Event s a)
planIOWeak  = planIO -- planIO' makeWeakRefKey

planIOWeakKey :: k -> Event s (Now s a) -> Now s (Event s a)
planIOWeakKey k =  planIO' (\_ y -> makeWeakRefKey k y)

flatPlan :: Event s (Now s a) -> Event s a
flatPlan Never = Never
flatPlan (E m) = memoE (flat m) where
 flat m = m >>= \case
            Right x -> EDone <$> x 
            Left Never -> return $ ESameAs Never
            Left (E m) -> return $ EMemoAgain (flat m)
planIO' makeRef Never = return Never
planIO' makeRef e = evNow e >>= \case
            Just n  -> pure <$> n
            Nothing -> do let e' = flatPlan e
                          p <- syncIO $ makeRef e' (Plan e')
                          addPlan p
                          return e'
addPlan :: Ref (Plan s) -> Now s ()
addPlan p = 
 do env <- getEnv
    pl <- syncIO $ readIORef (plans env)
    syncIO $ writeIORef (plans env) (p : pl)

addStrongRef :: MVar a  -> Now s ()
addStrongRef p = 
 do env <- getEnv
    syncIO $ modifyIORef' (strongRefs env) (SomeMVar p :) 


tryPlan :: (Plan s,  Ref (Plan s)) -> Now s ()
tryPlan (Plan (E n), r) = n >>= \case
         Right n  -> return ()
         Left e'  -> addPlan r

makeStrongRefs :: [Ref (Plan s)] -> Now s [(Plan s, Ref (Plan s))]
makeStrongRefs l = catMaybes <$> mapM makeStrongRef l where
  makeStrongRef r = syncIO (deRef r) >>= return . \case
                       Just p  -> Just (p,r)
                       Nothing -> Nothing

tryPlans :: Now s ()
tryPlans = 
  do env <- getEnv
     pl <- syncIO $ readIORef (plans env)
     syncIO $ writeIORef (plans env) []
     --syncIO $ putStrLn (show (length pl))
     pl' <- makeStrongRefs pl
     mapM_ tryPlan pl'

runFRPLocal :: (forall s. Now s (Event s a)) -> IO a
runFRPLocal m = withClock $ \c -> 
     do plans <- newIORef []
        strong <- newIORef []
        flag <- newFlag
        let env = Env c plans flag strong
        v <- runNow env m
        loop env v where
   loop env v = 
    runNow env (evNow v) >>= \case
         Just a -> return a
         Nothing -> 
           do waitForSignal (flag env)
              endRound (clock env) 
              runNow env tryPlans
              loop env v

runNow env m = runReaderT (runNow' m) env

-- Global stuff


data Global 

data FRPInit = forall a. FRPInit (Now Global (Event Global a)) (MVar a)

runInits :: MVar [FRPInit] -> Now Global ()
runInits inits = 
  do is <- syncIO $ swapMVar inits []
     mapM_ runInit is
  where runInit (FRPInit n m) = 
          do e <- n
             let setEm a = syncIO (putMVar m a)
             planIO (setEm <$> e)
             addStrongRef m

globalLoop :: Env Global -> MVar [FRPInit] -> IO ()
globalLoop env init = forever $ 
   do --putStrLn "Waiting"
      waitForSignal (flag env)
      endRound (clock env)  
      runNow env tryPlans
      --putStrLn "plans done"
      runNow env (runInits init)


global :: (Flag, MVar [FRPInit])
global = unsafePerformIO $ unsafeWithClock $ \c ->
    do flag <- newFlag
       init <- newMVar []
       plans <- newIORef []
       strong <- newIORef []
       let env = Env c plans flag strong
       forkIO $ globalLoop env init
       return (flag, init)
{-# NOINLINE global #-}  

runFRP :: Now Global (Event Global a) -> IO a
runFRP n = do m <- newEmptyMVar 
              let (flag,inits) = global
              is <- takeMVar inits
              putMVar inits (FRPInit n m : is)
              signal flag
              takeMVar m
              

