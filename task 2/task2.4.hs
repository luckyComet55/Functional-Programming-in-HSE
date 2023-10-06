digits :: Integer -> [Integer]
digits x
    | x < 0 = digits (-x)
    | x < 10 = [x]
    | otherwise = digits r ++ [d]
    where
        d = mod x 10
        r = div x 10

containsAllDigitsOnes :: Integer -> Bool
containsAllDigitsOnes x = (length (filter notZero (digits x))) == 9
    where
        notZero k = k /= 0