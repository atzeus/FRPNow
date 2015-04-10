

module Lib.EventStream
(Stream, next, nextSim, repeatEv, repeatIO, repeatIOList, merge, fmapB, filterJusts, filterEv, filterMapB, toChanges,
  filterB, during,sampleOn, scanlEv, filterStream, foldr1Ev, foldrEv, foldrSwitch, foldB, fold, parList, bufferStream, fromChanges, printAll)
  where

import Data.Maybe
import Control.Monad hiding (when)
import Control.Applicative hiding (empty)

import Data.Sequence hiding (reverse,scanl,take)
import Prelude hiding (until,length)
import Debug.Trace
import Control.Concurrent.MVar

import Swap
import Impl.FRPNow
import Lib.Lib

newtype Stream a = S { getEs :: Behavior (Event [a]) }

instance Functor Stream where
  fmap f (S b) = S $ (fmap f <$>) <$> b

next :: Stream a -> B (E a)
next (S s) = (head <$>) <$> s

nextSim :: Stream a -> Behavior (Event [a])
nextSim e = getEs e




repeatEv :: Behavior (Event a) -> Stream a
repeatEv b = S $ loop where
   loop = do e <- b
             let e' = (\x -> [x]) <$> e
             pure e' `switch` (loop <$ e)

-- in case of simultaneity, the left elements come first
merge :: Stream a -> Stream a -> Stream a
merge l r = loop where
  loop = S $
   do l' <- getEs l
      r' <- getEs r
      e <- fmap nxt <$> race l' r'
      let again = getEs loop
      pure e `switch` fmap (const again) e
  nxt (Tie  l r) = l ++ r
  nxt (L    l  ) = l
  nxt (R      r) = r



fmapB :: Behavior (a -> b) -> Stream a -> Stream b
fmapB f es = S $ loop where
 loop =  do e  <- getEs es
            plan (nxt <$> e)
 nxt l = (<$> l) <$> f


nextJusts :: Stream (Maybe a) -> Behavior (Event [a])
nextJusts es = loop where
  loop =
    do e <- getEs es
       join <$> plan (fmap nxt e)
  nxt l = case catMaybes l of
              [] -> loop
              l  -> return (return l)

filterStream :: (a -> Bool) -> Stream a -> Stream a
filterStream f s = filterJusts (toMaybef <$> s)
  where toMaybef x | f x = Just x
                   | otherwise = Nothing

filterJusts :: Stream (Maybe a) -> Stream a
filterJusts es = S $ nextJusts es

filterEv :: Stream Bool -> Stream ()
filterEv es = filterJusts (toJust <$> es)
  where toJust True = Just ()
        toJust False = Nothing


filterMapB :: Behavior (a -> Maybe b) -> Stream a -> Stream b
filterMapB f e = filterJusts $ fmapB f e

filterB :: Behavior (a -> Bool) -> Stream a -> Stream a
filterB f = filterMapB (toMaybe <$> f)
  where toMaybe f = \a ->  if f a then Just a else Nothing

during :: Stream a -> Behavior Bool -> Stream a
e `during` b = filterB (const <$> b) e

sampleOn :: Behavior a -> Stream x -> Stream a
sampleOn b s = S loop where
 loop = do e  <- getEs s
           let singleton x = [x]
           plan ((singleton <$> b) <$ e)


scanlEv :: (a -> b -> a) -> a -> Stream b -> Behavior (Stream a)
scanlEv f i es = S <$> loop i where
 loop i =
  do e  <- getEs es
     let e' = (\(h : t) -> tail $ scanl f i (h : t)) <$> e
     ev <- plan (loop . last <$> e')
     return (pure e' `switch` ev)

foldr1Ev :: (a -> Event b -> b) -> Stream a -> Behavior (Event b)
foldr1Ev f es = loop where
 loop =
  do e  <- getEs es
     plan (nxt <$> e)
 nxt [h]     = f h          <$> loop
 nxt (h : t) = f h . return <$> nxt t

foldrEv :: a -> (a -> Event b -> b) -> Stream a -> Behavior b
foldrEv i f es = f i <$> foldr1Ev f es

foldrSwitch :: Behavior a -> Stream (Behavior a) -> Behavior (Behavior a)
foldrSwitch b = foldrEv b switch

foldBs :: Behavior a -> (Behavior a -> b -> Behavior a) -> Stream b -> Behavior (Behavior a)
foldBs b f es = scanlEv f b es >>= foldrSwitch b

repeatIO :: IO a -> Now (Stream a)
repeatIO m = S <$> loop where
  loop = do  h  <- async m
             t  <- planNow (loop <$ h)
             return (pure ((\x -> [x]) <$> h) `switch` t)

repeatIOList :: IO [a] -> Now (Stream a)
repeatIOList m = S <$> loop where
  loop = do  h  <- async m
             t  <- planNow (loop <$ h)
             return (pure h `switch` t)

catMaybesStream :: Stream (Maybe a) -> Stream a
catMaybesStream (S s) = S $ loop where
  loop = do  e <- s
             join <$> plan (nxt <$> e)
  nxt l = case  catMaybes l of
             [] -> loop
             l  -> return (return l)

snapshots :: B a -> Stream () -> Stream a
snapshots b (S s) = S $
  do  e       <- s
      ((\x -> [x]) <$>) <$> snapshot b (head <$> e)

fold :: (a -> b -> a) -> a -> Stream b -> Behavior (Behavior a)
fold f i s = loop i where
  loop i = do e  <- getEs s
              let e' = foldl f i <$> e
              ev <- plan (loop <$> e')
              return (i `step` ev)

parList :: Stream (BehaviorEnd b ()) -> Behavior (Behavior [b])
parList = foldBs (pure []) (flip (.:))

bufferStream :: Int -> Stream a -> Behavior (Stream [a])
bufferStream i = scanlEv (\t h -> take i (h : t)) []

fromChanges :: Eq a => Behavior a -> Stream a
fromChanges = repeatEv . changeVal

toChanges :: a -> Stream a -> Now (Behavior a)
toChanges i (S x) = loop i where
  loop i = do e  <- sample x
              e' <- plan (loop . last <$> e)
              return (i `step` e')




-- give an event stream that has an event each time the
-- returned function is called
-- useful for interfacing with callback-based
-- systems
callbackStreamSim :: Now (Stream a, [a] -> IO ())
callbackStreamSim = init where
  init = do cbm <- unsafeSyncIO $ newEmptyMVar
            b <- changeCallback cbm
            return (S b, func cbm)

  changeCallback :: MVar ([a] -> IO ()) -> Now (Behavior (Event [a]))
  changeCallback cbm =
     do (ev,cb) <- callbackE
        unsafeSyncIO $ putMVar cbm cb
        ev'<- planNow (changeCallback cbm <$ ev)
        return (ev `step` ev')

  func :: MVar ([a] -> IO ()) -> [a] -> IO ()
  func mv x = do f <- takeMVar mv
                 f x

callbackStream :: Now (Stream a, a -> IO ())
callbackStream = do (s,cb) <- callbackStreamSim
                    return (s, cb . (\x -> [x]))

-- call the given function each time an event occurs
callStream :: ([a] -> Now (Event ())) -> Stream a -> Now ()
callStream f evs = do e2 <- sample (nextSim evs)
                      planNow (again <$>  e2)
                      return () where
  again a = do e2 <- f a
               e <- sample (nextSim evs)
               planNow (again <$> (e2 >> e))
               return ()

callIOStream :: (a -> IO a) -> Stream a -> Now ()
callIOStream f = callStream (\x -> async (mapM_ f x))               

printAll :: (Show a, Eq a) => Stream a -> Now ()
printAll = callStream (\l -> unsafeSyncIO $ mapM_ (putStrLn . show) l >> return (pure ()))

{-
-- See reflection without remorse for which performance problem this construction solves...

type Evs     x = Seq (EvsView x)
type EvsView x = Event (EH x)
data EH x = x :| Evs x | End


toView :: Evs x -> EvsView x
toView e = case viewl e of
     EmptyL -> return End
     h :< t -> h >>= nxt t
  where nxt t End = toView t
        nxt r (h :| l) = let q = l >< r in return (h :| q)

append :: Evs x -> Evs x -> Evs x
append = (><)

app :: Evs x -> Event (Evs x) -> Evs x
app l r = append l (singleton $ join $ fmap toView r) -- it's magic!

emptyEmits = empty
singleEmit x = singleton (return (x :| emptyEmits))

toStream :: Evs x -> Stream x
toStream = Es . loop where
  loop e = do e' <- lose e
              eh <- join <$> plan (nxt [] <$> toView e')
              pure eh `switch` (loop e' <$ eh)
  lose e = getNow (toView e) >>= \case
            Just End      -> return emptyEmits
            Just (h :| t) -> lose t
            Nothing       -> return e
  nxt :: [x] -> EH x -> Behavior (Event ([x]))
  nxt [] End     = return never
  nxt l  End     = return (return (reverse l))
  nxt l (h :| t) = getNow (toView t) >>= \case
                    Just x -> nxt (h : l) x
                    Nothing -> return (return (reverse (h: l)))

data StreamM x a = StreamM { emits :: Evs x, eend :: Event a }

instance Monad (StreamM x) where
  return x = StreamM emptyEmits (return x)
  (StreamM s e) >>= f = let fv = fmap f e
                                 fs = emits <$> fv
                                 fa = fv >>= eend
                             in StreamM (app s fs) fa

emit :: (Swap (BehaviorEnd x) f, Monad f) =>  x -> (f :. StreamM x) ()
emit x = liftRight $ StreamM (singleEmit x) (return ())

instance Wait (StreamM x) where
  waitEv = StreamM emptyEmits

instance (Monad b, Swap Event b) => Swap (StreamM x) b where
  swap (StreamM b e) = liftM (StreamM b) (plan e)

instance Functor (StreamM x) where fmap = liftM
instance Applicative (StreamM x) where pure = return ; (<*>) = ap

runStreamM :: StreamM x a -> (Stream x, Event a)
runStreamM (StreamM s e) = (toStream s, e)


-}
