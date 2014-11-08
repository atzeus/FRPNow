{-# LANGUAGE  DeriveFunctor, GeneralizedNewtypeDeriving #-}


module Behaviour where

import Past
import Event
import System.IO.Unsafe
import Data.IORef
import Control.Monad
import Control.Applicative
import Control.Monad.Fix

data Behaviour a = Behaviour { after :: PastTime -> (a, Event (Behaviour a)) } 

instance Monad Behaviour where
  return x = Behaviour $ const (x,never)
  m >>= f  = Behaviour $ \t -> 
      let (a,e) = m `after` t
          b     = f a `switch` fmap (>>= f) e
      in b `after` t

switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch b e = Behaviour $ \t -> 
  case e `infoAt` t of
     Just (_,b') -> b' `after` t
     Nothing     -> let (h,tl) = b `after` t
                    in (h, fmap (`switch` e) tl)

whenJust :: Behaviour (Maybe a) -> Behaviour (Event a)
whenJust b = Behaviour $ \t -> 
   case b `after` t of
     (Just x, tl) -> (delay t (return x), fmap whenJust tl)
     (Nothing, t) -> let t' = cont $ fmap whenJust t
                     in (t' >>= fst, t' >>= snd) 
 where cont :: Event (Behaviour a) -> Event (a, Event (Behaviour a))
       cont e = fmap (\(t,b) -> b `after` t) $ withTime e
                              
seqB :: Behaviour x -> Behaviour a -> Behaviour a
seqB s r = Behaviour $ \t -> 
   (s `after` t) `seq` (r `after` t)

memoB :: (PastTime -> (a, Event (Behaviour a))) -> Behaviour a
memoB f = Behaviour $ unsafePerformIO $ liftM f' (newIORef (bigBang, f bigBang)) where
  f' r t = unsafePerformIO $
      do (pt,(hd,tl)) <- readIORef r
         case compare pt t of
           EQ -> return (hd,tl)
           GT -> error "Non-monotomic sampling!"
           LT -> let (hd',_) = f t
                     tl'      = dropEvents t tl
                     res = (hd',tl')
                 in writeIORef r (t,res) >> return res
  dropEvents :: PastTime -> Event (Behaviour a) -> Event (Behaviour a)
  dropEvents t e = case e `infoAt` t of
        Just (x,b) -> dropEvents t (snd (b `after` t))
        Nothing -> e
               
         
