import System.IO.Unsafe


forget :: Ord index => (index -> x -> x) -> (x -> a) -> x -> (index -> a)
forget drop head init = unsafePerformIO $ loopy <$> newIORef (init, Nothing, undefined)
  loopy ref index = unsafePerformIO $ 
      do (cur, previ,cv) <- readIORef ref
         case  previ `compare` index of
            LT -> let new = drop index cur
                      newVal = head new
                  in do writeIORef ref (new, Just index, newVal)
                        return newVal
            EQ -> return cv
            GT -> error "Non increasing access to forget!"
