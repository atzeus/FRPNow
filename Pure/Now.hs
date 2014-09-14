module Pure.Now where

import Control.Monad.Reader

import Pure.Event
import Pure.Behaviour

type Now a = Reader Time a

plan :: Event (Now a) -> Now (Event a)
plan (Event t a) = fmap runThen ask
  where runThen tn = let t' = max tn t
                     in Event t' (runReader a t')

hasOccured :: Event a -> Now (Maybe a)
hasOccured (Event te a) = fmap toJust ask
  where toJust t 
          | t >= te   = Just a
          | otherwise = Nothing 

now :: Behaviour a -> Now a
now (h :| t) = do tv <- hasOccured t
                  case tv of 
                    Just a  -> now a
                    Nothing -> return h
