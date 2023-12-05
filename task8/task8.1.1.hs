data Result a = Ok a | Error String
    deriving (Eq, Show)

instance Functor Result where
    fmap f (Ok a) = Ok (f a)
    fmap f (Error s) = Error s

instance Foldable Result where
    foldMap f (Error s) = mempty
    foldMap f (Ok x) = f x

instance Traversable Result where
    traverse f (Error s) = pure $ Error s
    traverse f (Ok x) = Ok <$> f x