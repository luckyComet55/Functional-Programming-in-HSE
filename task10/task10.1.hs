import Control.Monad (ap)
-- Рукописный логгер
data Logged a = Logged String a deriving (Eq,Show)

instance Functor Logged where
  fmap f (Logged s a) = Logged s (f a)

instance Applicative Logged where
  pure = Logged ""
  (<*>) = ap

instance Monad Logged where
  return = pure
  (>>=) (Logged s a) f = Logged (s' ++ s) y
                        where
                            Logged s' y = f a

-- эквивалент tell
write2log :: String -> Logged ()
write2log str = Logged str ()

logIt v = do
    write2log $ "var = " ++ show v ++ "; "
    return v

test = do
    x <- logIt 3
    y <- logIt 5
    let res = x + y
    write2log $ "sum = " ++ show res ++ "; "
    return res