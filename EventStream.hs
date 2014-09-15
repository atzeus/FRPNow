module EventStream(EventStream, foldES, foldESPure, nextEvs, nextEv, unfoldTime) where

import IO.Now
import Lib
import Data.Maybe
import Control.Monad
import Control.Applicative


newtype EventStream s a = ES (Behaviour s (EVS s a))


type EVS s a = Event s (ESHT s a)
data ESHT s a = a :| Event s (ESHT s a)

esToBehaviour :: EVS s a -> Behaviour s (EVS s a)
esToBehaviour e = pure e `switch` (e >>= \(_ :| t) -> return $ esToBehaviour t)

-- most general..
foldES :: (Behaviour s a -> b -> Now s (Behaviour s a)) -> Behaviour s a -> EventStream s b -> Now s (Behaviour s a)
foldES f i es = loopI i where
  loopI i = do ev <- nextEvs es
               evn <- plan $ fmap (loop i) ev
               return (i `switch` evn)
  loop i [] = loopI i
  loop i (h : t) = do i' <- f i h ; loop i' t
              
-- simpler 
foldESPure :: (a -> b -> a) -> a -> EventStream s b -> Now s (Behaviour s a)
foldESPure f i es = foldES (liftEm f) (pure i) es where
  liftEm f a b = do a' <- now a
                    return (pure $ f a' b)
               
nextEv :: EventStream s a -> Now s (Event s a)
nextEv (ES es) = do e <- now es; return (fmap (\(a :| _) -> a) e)

-- deals with simultanianity
nextEvs :: EventStream s a -> Now s (Event s [a])
nextEvs (ES es) = do e <- now es; plan (fmap gather e)
  where gather (h :| t) = do tm <- cur t
                             case tm of
                               Just tv -> do tvs <- gather tv
                                             return (h : tvs)
                               Nothing -> return [h]

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
             
                                     
