drop' :: Int -> [a] -> [a]
drop' n xs = foldr step ini xs n

step :: a -> (Int -> [a]) -> Int -> [a]
step e g n
    | n <= 0 = e : g 0
    | otherwise = g (n - 1)

ini :: Int -> [a]
ini _ = []