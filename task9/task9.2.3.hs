factor2 :: Integer -> [(Integer, Integer)]
factor2 x = do
            a <- filter (\n -> mod x n == 0) [1 .. round (sqrt (fromIntegral x))]
            return (a, div x a)