data ST s a = ST (s -> Maybe (s, a)) 

unST (ST f) = f

instance Functor (ST s) where
  fmap f (ST s) = ST (\sd -> case unST (ST s) sd of
                                Nothing -> Nothing
                                Just (ss, x) -> Just (ss, f x))

instance Applicative (ST s) where
  pure a = ST (\s -> Just (s, a))
  ST f <*> ST x = ST (\s -> case unST (ST f) s of
                                Nothing -> Nothing
                                Just (ss, g) -> unST (fmap g (ST x)) ss)

instance Monad (ST s) where
   return x = ST (\s -> Just (s, x))
   m >>= k = ST (\s -> case (unST m s) of
                            Nothing -> Nothing
                            Just (s', v) -> unST (k v) s')
   fail _ = ST (\s -> Nothing)
   
getState :: ST s s
getState = ST (\s -> Just (s,s))

setState :: s -> ST s ()
setState sn = ST (\s -> Just (sn, ()))

runST :: ST s a -> s -> Maybe a
runST m s = fmap snd (unST m s)

numsToStrings n | n `mod` 2 == 0 = "Even was here"
               | otherwise = "Odd was here"

stringify nv = fmap numsToStrings (return nv)

testState = ST (\s -> Just (s, 0))

testBind = unST((>>=) testState stringify) "A new state"

main = do line <- getLine   
          let line' = reverse line  
          putStrLn $ "You said " ++ line' ++ " backwards!"  
          putStrLn $ "Yes, you really said" ++ line' ++ " backwards!"  