{-# LANGUAGE ConstraintKinds, TypeFamilies,MultiParamTypeClasses, FlexibleInstances, TypeOperators,LambdaCase,UndecidableInstances,FunctionalDependencies #-}

module BehaviorM where
import Control.Monad
import Control.Applicative
import FRPClasses
import EventM


type BehaviorState m a = (a, EventM m (BehaviorM m a))
data BehaviorM m a = B { runBehavior ::  m (BehaviorState m a) }

againB :: Monad m => BehaviorState m a -> m (BehaviorState m a)
againB (h, t) = tryEv t >>= \case
                 Left m -> return (h,m)
                 Right x -> runBehavior x

class Monad m => Plan m e where
  plan     :: e m (m a) -> m (e m a)


instance Plan m EventM => Monad (BehaviorM m) where
  return x = B $ return (x, never)
  m >>= f  = B $  
     do (h,t) <- runBehavior m
        runBehavior $ f h `switch` ((>>= f) <$> t)


instance Plan m EventM => Behavior (BehaviorM m) (EventM m) where
  switch b e = B $ tryEv e >>= \case
    Right x  -> runBehavior x
    Left e   -> do (h,t) <- runBehavior b
                   return (h, ((`switch` e) <$> t) .|. e)

  whenJust b = B $ 
   do (h, t) <- runBehavior b
      case h of
       Just x -> return (return x, whenJust <$> t)
       Nothing -> do en <- plan (runBehavior . whenJust <$> t)
                     return (en >>= fst, en >>= snd)



{-

instance Snatch BehaviorM where
  snatch m = B $ m >>= runBehavior
                
instance Bind BehaviorM where
  memo (B m) = liftM B (memoAgain againB m)
-}

