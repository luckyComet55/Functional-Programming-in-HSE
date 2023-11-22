import Control.Applicative

newtype Parser a = Parser { apply :: String -> [(a, String)] }

instance Functor Parser where
    fmap g p = Parser f
        where
            f s = case apply p s of
                [(x, xs)] -> [(g x, xs)]
                [] -> []

instance Applicative Parser where
    pure x = Parser (\s -> [(x, s)])
    (<*>) (Parser p1) (Parser p2) = Parser f
        where
            f xs = [(g x, xs1) | (g, xs2) <- p1 xs, (x, xs1) <- p2 xs2]

instance Alternative Parser where
    empty = Parser (const [])
    (<|>) (Parser p1) (Parser p2) = Parser f where
        f xs = case p1 xs of
            [] -> case p2 xs of
                [] -> []
                z  -> z
            z' -> z' ++ case p2 xs of
                []  -> []
                z'' -> z''