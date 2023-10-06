digits :: Integer -> [Integer]
digits x
    | x < 0 = digits (-x)
    | x < 10 = [x]
    | otherwise = digits r ++ [d]
    where
        d = mod x 10
        r = div x 10

dgs = [1..9]
containsAllDigits :: Integer -> Bool
containsAllDigits x = and (map (elem') dgs)
    where
        elem' k = k `elem` digits x