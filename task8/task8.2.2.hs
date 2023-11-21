data Tree a = Nil | Branch (Tree a) a (Tree a) deriving (Eq, Show)

instance Foldable Tree where
    foldr _ z Nil = z
    foldr f z (Branch tl a tr) = foldr f res tl
        where
            res = f a res'
            res' = foldr f z tr

instance Functor Tree where
    fmap _ Nil = Nil
    fmap f (Branch tl a tr) = Branch (fmap f tl) (f a) (fmap f tr)

instance Applicative Tree where
    pure a = Branch (pure a) a (pure a)

    (<*>) Nil _ = Nil
    (<*>) _ Nil = Nil
    (<*>) (Branch fTl f fTr) (Branch tl x tr) = Branch (fTl <*> tl) (f x) (fTr <*> tr)

instance Traversable Tree where
    traverse _ Nil = pure Nil
    traverse f (Branch tl v tr) = Branch <$> traverse f tl <*> f v <*> traverse f tr

    sequenceA = traverse id