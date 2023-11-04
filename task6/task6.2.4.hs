infixl 9 !!!

(!!!) :: [a] -> Int -> Maybe a
xs !!! n = foldr fun ini xs n

fun :: a -> (Int -> Maybe a) -> Int -> Maybe a
fun c g n
    | n > 0 = g (n - 1)
    | n == 0 = Just c
    | otherwise = Nothing

ini :: Int -> Maybe a
ini _ = Nothing