tails' :: [a] -> [[a]]
tails' = foldr fun ini

fun :: a -> [[a]] -> [[a]]
fun c b = (c : head b) : b
ini = [[]]

inits' :: [a] -> [[a]]
inits' = foldr fun' ini'

fun' :: a -> [[a]] -> [[a]]
fun' c b = [] : map (c:) b
ini' = [[]]