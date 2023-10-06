digits :: Integer -> [Integer]
digits x
    | x < 0 = digits (-x)
    | x < 10 = [x]
    | otherwise = digits r ++ [d]
    where
        d = mod x 10
        r = div x 10