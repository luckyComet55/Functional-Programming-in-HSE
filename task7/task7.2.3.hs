data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch tl a tr) = Branch (fmap f tl) (f a) (fmap f tr)

instance Applicative Tree where
    pure a = Branch (pure a) a (pure a)

    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Branch fTl f fTr) (Branch tl x tr) = Branch (fTl <*> tl) (f x) (fTr <*> tr)