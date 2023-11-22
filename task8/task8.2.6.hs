class Functor f => Monoidal f where
    unit  :: f ()
    (*&*) :: f a -> f b -> f (a,b)

instance Monoidal Maybe where
    unit = Just ()
    (*&*) Nothing _ = Nothing
    (*&*) _ Nothing = Nothing
    (*&*) (Just a) (Just b) = Just (a, b)

instance Monoid s => Monoidal ((,) s) where
    unit = (mempty, ())
    (*&*) (a, b) (c, d) = (a <> c, (b, d))

instance Monoidal ((->) e) where
    unit e = ()
    (*&*) a b e = (a e, b e)