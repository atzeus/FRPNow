{-# LANGUAGE FlexibleContexts, ExistentialQuantification, Rank2Types,GeneralizedNewtypeDeriving  #-}
module FRPNow(E,B,Now, never, switch, whenJust, async, sample, planNow, runNow) where
import Control.Applicative
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Control.Monad.Reader  hiding (mapM_)
import Control.Monad.Writer  hiding (mapM_)
import Data.IORef
import Ref
import Data.Sequence
import System.IO.Unsafe
import Data.Foldable
import PrimEv

import Prelude hiding (mapM_)

-- Section 6.1
data E a  = E { runE :: M (Either (E a) a) }

never = E $ return (Left never)

instance Monad E where
  return  x = E $ return (Right x)
  m >>= f = memoE (m `bindLeakE` f) -- this in section 6.2

m `bindLeakE` f = E $ 
    runE m >>= \r -> case r of
                      Right x  ->  runE (f x)
                      Left e'  ->  return (Left (e' `bindLeakE` f))

minTime :: E x -> E y -> E ()
minTime l r  = E (merge <$> runE l <*> runE r) where
  merge (Right _)  _            = Right ()
  merge _          (Right _  )  = Right ()
  merge (Left l')  (Left r'  )  = Left (minTime l' r')

-- Section 6.2

unrunE :: Either (E a) a -> E a 
unrunE (Left e)    = e
unrunE (Right a)   = pure a

memoEIO :: E a -> IO (E a)
memoEIO einit = 
  do r <- newIORef einit 
     return (usePrevE r)

usePrevE :: IORef (E a) -> E a
usePrevE r = E $ 
  do e <- liftIO (readIORef r)
     res <- runE e
     liftIO (writeIORef r (unrunE res))
     return res

memoE :: E a -> E a
memoE e = e
--memoE e = unsafePerformIO $ memoEIO e
  
-- Section 6.3

data B a = B { runB :: M (a, E (B a)) }

switchLeak ::  B a -> E (B a) -> B a
switchLeak b e     = B $
  runE e >>= \r -> case r of
        Right x   -> runB x
        Left  e'  -> do  (h,t) <- runB b
                         return (h, switchE t e')

switchE :: E (B a) -> E (B a) -> E (B a)
switchE l r =  ((pure undefined `switchLeak` l) `switchLeak` r) <$ 
               minTime l r

joinBLeak :: B (B a) -> B a
joinBLeak m = B $ 
    do  (h,t) <- runB m
        runB $ h `switchLeak` (joinBLeak <$> t)

fmapLeak f (B b) = B $ 
       do (h,t) <- b
          return (f h, fmap (fmap f) t)

bindLeakB :: B a -> (a -> B b) -> B b
m `bindLeakB` f = joinBLeak (fmap f m)

whenJustLeak :: B (Maybe a) -> B (E a)
whenJustLeak b = B $ 
    do  (h, t) <- runB b
        case h of
         Just x -> return (return x, whenJustLeak <$> t)
         Nothing -> 
          do  en <- planM (runB . whenJustLeak <$> t)
              return (en >>= fst, en >>= snd)

instance Functor B where
  fmap f b = memoB (fmapLeak f b)

instance Monad B where
  return x = B $ return (x, never)
  m >>= f = memoB (m `bindLeakB` f)


whenJust :: B (Maybe a) -> B (E a)
whenJust b = memoB (whenJustLeak b)

switch :: B a -> E (B a) -> B a
switch b e = memoB (switchLeak b e)

-- Section 6.4

unrunB :: (a,E (B a)) -> B a 
unrunB (h,t) = B $ 
  runE t >>= \x -> case x of
        Right b -> runB b
        Left t' -> return (h,t')

memoBIO :: B a -> IO (B a)
memoBIO einit = 
  do r <- newIORef einit 
     return (usePrevB r)

usePrevB :: IORef (B a) -> B a
usePrevB r = B $ 
  do b <- liftIO (readIORef r)
     res <- runB b
     liftIO (writeIORef r (unrunB res))
     return res
     
memoB :: B a -> B a
memoB b = b
--memoB b = unsafePerformIO $ memoBIO b

-- Section 6.7




type M = WriterT Plans (ReaderT Clock IO)

newtype Now a = Now { getNow :: M a } deriving (Functor,Applicative,Monad)

sample :: B a -> Now a
sample (B m) = Now $ fst <$> m

async :: IO a -> Now (E a)
async m = Now $ do  c <- ask
                    toE <$> liftIO (spawn c m)

toE :: PrimEv a -> E a
toE p = E (toEither . (p `observeAt`) <$> getRound) 
  where  toEither Nothing   = Left (toE p)
         toEither (Just x)  = Right x
getRound :: (MonadReader Clock m, MonadIO m) => m Round
getRound = ask >>= liftIO . curRound 

data Plan a = Plan (E (M a)) (IORef (Maybe a))

planToEv :: Plan a -> E a
planToEv (Plan ev ref) = E $
  liftIO (readIORef ref) >>= \pstate -> 
  case pstate of
   Just x   -> return (Right x)
   Nothing  -> runE ev >>= \estate ->
    case estate of
     Left ev' -> 
         return $ Left $ planToEv (Plan ev' ref)
     Right m  -> do  v <- m
                     liftIO $ writeIORef ref (Just v)
                     return $ Right v

data SomePlan = forall a. SomePlan (Ref (Plan a))
type Plans = Seq SomePlan

planM :: E (M a) -> M (E a)
planM e = plan makeWeakRef e

planNow :: E (Now a) -> Now (E a)
planNow e = Now $ plan makeStrongRef (getNow  <$> e)

plan :: (forall x. x -> IO (Ref x)) -> E (M a) -> M (E a)
plan makeRef e = 
  do p <- Plan e <$> liftIO (newIORef Nothing)
     pr <- liftIO (makeRef p)
     addPlan pr
     return (planToEv p)

addPlan :: Ref (Plan a) -> M ()
addPlan = tell . singleton . SomePlan

runNow :: Now (E a) -> IO a 
runNow (Now m) = do c <- newClock 
                    runReaderT (runWriterT m >>= mainLoop) c 

mainLoop :: (E a, Plans) -> ReaderT Clock IO a
mainLoop (ev,pl) = loop pl where
  loop pli =
    do  (er,ple) <- runWriterT (runE ev) 
        let pl = pli >< ple
        case er of
          Right x   -> return x
          Left _    -> do  endRound
                           pl' <- tryPlans pl
                           loop pl' 
  
endRound :: ReaderT Clock IO ()
endRound = ask >>= liftIO . waitEndRound 
  
tryPlans :: Plans -> ReaderT Clock IO Plans
tryPlans pl =snd  <$> runWriterT 
                  (mapM_ tryPlan pl) where
  tryPlan (SomePlan pr) = 
   do  ps <-  liftIO (deRef pr) 
       case ps of
        Just p -> do  eres <- runE (planToEv p)
                      case eres of
                       Right x -> return ()
                       Left _  -> addPlan pr
        Nothing -> return ()

instance Applicative B where
  pure = return
  (<*>) = ap
                 
instance Functor E where
  fmap = liftM

instance Applicative E where
  pure = return
  (<*>) = ap
