data NEList a = Single a | Cons a (NEList a)
    deriving (Eq, Show)

instance Functor NEList where
    fmap f (Single a) = Single (f a)
    fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Foldable NEList where
    foldr f z (Single a) = f a z
    foldr f z (Cons a as) = foldr f (f a z) as

    foldMap f (Single a) = f a
    foldMap f (Cons a as) = f a <> foldMap f as

instance Traversable NEList where
    sequenceA (Single a) = Single <$> a
    sequenceA (Cons a as) = Cons <$> a <*> sequenceA as