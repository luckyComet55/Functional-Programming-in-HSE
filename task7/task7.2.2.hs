data Triple a = Tr a a a deriving (Eq,Show)

instance Functor Triple where
    fmap :: (a -> b) -> Triple a ->  Triple b
    fmap f (Tr x y z) = Tr (f x) (f y) (f z)

instance Applicative Triple where
    pure :: a -> Triple a
    pure a = Tr a a a

    (<*>) :: Triple (a -> b) -> Triple a -> Triple b
    (<*>) (Tr f g h) (Tr x y z) = Tr (f x) (g y) (h z)