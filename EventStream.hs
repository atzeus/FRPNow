{-# LANGUAGE GADTs, TupleSections #-}

module EventStream(EventStream)where

import IO.Implementation
import Lib
import Data.Maybe
import Control.Monad
import Control.Applicative
import TermM

      


{-
-- simpler 
foldESPure :: (a -> b -> a) -> a -> EventStream s b -> Now s (Behaviour s a)
foldESPure f i es = foldES (liftEm f) (pure i) es where
  liftEm f a b = do a' <- now a
                    return (pure $ f a' b)
               


unfoldTime :: (a -> Now s (Event s a)) -> a -> Event s () -> Now s (EventStream s a)
unfoldTime f i end = 
  do e <- loop i
     let r = esToBehaviour e
     ensureEvalB r -- prevent space leak!
     return (ES r)
  where loop i =
          do m <- cur end
             if isJust m
             then return never
             else do h <- f i
                     t <- plan $ fmap loop h 
                     return (fmap (\x -> x :| join t) h)
-}
             
                                     
