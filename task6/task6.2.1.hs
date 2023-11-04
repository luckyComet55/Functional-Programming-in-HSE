import Data.List (unfoldr)
revRange :: (Char,Char) -> [Char]
revRange = unfoldr fun

fun (f, t) = if f > t then Nothing else Just (t, (f, pred t))