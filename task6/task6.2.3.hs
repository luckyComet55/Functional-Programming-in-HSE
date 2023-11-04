reverse' :: [a] -> [a]
reverse' = foldr fun' ini'
fun' c b = b ++ [c]
ini' = []

reverse'' :: [a] -> [a]
reverse'' = foldl fun'' ini''
fun'' b c = c : b
ini'' = []