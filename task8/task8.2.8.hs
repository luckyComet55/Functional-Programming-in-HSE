class Functor f => Monoidal f where
    unit  :: f ()
    (*&*) :: f a -> f b -> f (a,b)

pure' :: Monoidal f => a -> f a
pure' a = fmap (const a) unit

ap' :: Monoidal f => f (a -> b) -> f a -> f b
ap' a b = fmap (\(x, y) -> x y) (a *&* b)