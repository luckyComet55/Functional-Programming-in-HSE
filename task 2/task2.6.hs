repeatEveryElem :: Int -> [a] -> [a]
repeatEveryElem factor [] = []
repeatEveryElem factor (x:xs) = [x | _ <- [1..factor]] ++ repeatEveryElem factor xs