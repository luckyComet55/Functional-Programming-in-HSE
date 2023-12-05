absDiff :: Num a => [a] -> [a]
absDiff xs = do
            res <- zipWith (-) as bs
            return (abs res)
            where
                as | null xs = []
                   | otherwise = init xs
                bs | null xs = []
                   | otherwise = tail xs