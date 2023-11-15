divideList' :: (Show a, Fractional a) => [a] -> (String,a)
divideList' []     = ("1.0", 1.0)
divideList' (x:xs) = (/) <$> ("<-" ++ show x ++ "/", x) <*> divideList' xs
