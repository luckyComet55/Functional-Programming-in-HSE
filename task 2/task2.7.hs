movingLists :: Int -> [a] -> [[a]]
movingLists n l
    | len' < n = []
    | otherwise = sublist : movingLists n (tail l)
    where
        sublist = take n l
        len' = length sublist