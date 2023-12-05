import Control.Monad.Writer
minusLoggedR :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedR x xs | null xs = writer (x, show x)
                  | otherwise = do
                    let res = head xs - fst (runWriter $ minusLoggedR x (tail xs))
                    let a = execWriter (listen (minusLoggedR x (tail xs)))
                    tell ("(" ++ show (head xs) ++ "-" ++ a ++ ")")
                    return res