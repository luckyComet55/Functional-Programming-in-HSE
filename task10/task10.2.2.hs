import Control.Monad.Writer
minusLoggedL :: (Show a, Num a) => a -> [a] -> Writer String a
minusLoggedL x = go (writer (x, show x))
            where
                go e [] = e
                go e es = go (writer (ans - head es, "(" ++ a ++ "-" ++ show (head es) ++ ")")) $ tail es
                    where
                        ans = fst $ runWriter e
                        a = execWriter (listen e)