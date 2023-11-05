foldl'' :: (b -> a -> b) -> b -> [a] -> b
foldl'' f v xs = foldr (fun f) ini xs v

fun :: (b -> a -> b) -> a -> (b -> b) -> b -> b
fun fActual valRight idFunc valLeft = idFunc (fActual valLeft valRight)

ini :: a -> a
ini = id