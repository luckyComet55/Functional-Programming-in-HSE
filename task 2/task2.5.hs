sublist :: Int -> Int -> [a] -> [a]
sublist a b l
    | a > b = []
    | a < 0 = sublist 0 b l
    | b < 0 = []
    | otherwise = take (b - a) (drop a l)