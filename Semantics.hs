module Semantics where

import Control. Applicative

type Behaviour a = Time -> a
type Event a = (Time,a)

-- reader monad
instance Applicative (Behaviour a) -- reader
  pure = const
  f <*> x = \t -> f t (x t))

instance Monad (Event a) -- writer monad
  return a = (-inf,a)
  fmap f (t,a) = (t, f a)
  join (t,(t2,a)) = (max t t2, a)

switch :: Behaviour a -> Event (Behaviour a) -> Behaviour a
switch b (ts,b2) t 
   | t < ts    = b t
   | otherwise = b2 t

type SpaceTime = Behaviour World -- Time -> World -- Sausage
type Now = ReaderMonad Time (StateMonad SpaceTime)   

now :: Behaviour a -> Now a
now f = f <$> getTime

whenJust :: Behaviour (Maybe a) -> Now (Event a)
whenJust f t = do 	t <- getTime
					let t2 = magicAnalyze (fmap isJust f) t
                    return (t2, fromJust $ f t2) 


magicAnalyze :: Behaviour Bool -> Behaviour Time
magicAnalyze = undefined
-- given a behaviour f and a time t1, find the time t2 , with
-- t2 >= t1, such that t2 is the minimal time such that 
-- such that f t2 is True (+ continuous time nastiness)

act :: IO a -> Now (Event a)
act  = toSpaceTimeChange 

plan :: Event (Now a) -> Now (Event a)
plan (t,n) = lift (runReader t n)



-- change spacetime by planning IO a action at the given time
toSpaceTimeChange :: IO a -> Time -> SpaceTime -> (SpaceTime, Event a) 
toSpaceTimeChange = undefined
{-
type ClockVal = Double 

clock :: Behaviour ClockVal
-- when sampling interval goes to zero, in the limit, clock == (time :: Behaviour Time)



-- interuptable I/O




koen :: Behaviour Int -> Behaviour Int
koen a = (+) <$> a <*> (fmap (+1) a)




-- mousePos example
mousePos :: Point -> Now (Behaviour Point)
mousePos i = do pe <- act getMousePos -- getMousePos is blocking I/O action
                se <- plan (fmap mousePos pe)  
                return (pure i `switch` se)

saveOnMouseOver :: Behaviour Picture -> Rect -> Behaviour Point -> Now ()
saveOnMouseOver p r b = do e <- now $ when (isInside r <$> b)
                           plan (fmap (const (savePict p)) e)
                           return () 

savePict :: Behaviour Picture -> Now ()
savePict b = do p <- now b
                act (writeStuff p)
                return ()
                -- writeStuff :: Picture -> IO () 

elapsed :: Behaviour ClockVal -> Now (Behaviour ClockVal)
elapsed c = do tstart <- now c 
               return $ (- tstart) <$> c



race :: Event a -> Event b -> Now (Event (Either a b)) 
race a b = now $ whenJust $ fmap choose <$> toBehav a <*> toBehav b
  where toBehav :: Event a -> Behaviour (Maybe a)
        toBehav a = pure Nothing `switch` fmap (pure . Just) a 

  	choose (Just a, _)       = Left a
        choose (Nothing, Just b) = Right b
-}
