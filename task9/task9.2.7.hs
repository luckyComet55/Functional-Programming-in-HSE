data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

instance Functor OddC where
    fmap f (Un a) = Un (f a)
    fmap f (Bi a b c) = Bi (f a) (f b) (fmap f c)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) t = Bi x y t
concat3OC (Bi x y z) a t = Bi x y $ concat3OC z a t
concat3OC (Un x) (Bi y z e) t = Bi x y $ concat3OC (Un z) e t

instance Applicative OddC where
    pure = Un

    (<*>) (Un f) (Un a) = Un (f a)
    (<*>) (Un f) (Bi x y z) = Bi (f x) (f y) (fmap f z)
    (<*>) (Bi f g h) (Un a) = Bi (f a) (g a) (h <*> pure a)
    (<*>) (Bi f g h) (Bi x y z) = concat3OC (fmap f (Bi x y z)) (fmap g (Bi x y z)) (h <*> Bi x y z)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi (Un x) (Un y) z) = Bi x y (concatOC z)
concatOC (Bi (Un x) (Bi y z a) k) = Bi x y (concatOC (Bi (Un z) a k))
concatOC (Bi (Bi x y z) e k) = Bi x y (concatOC (Bi z e k))

instance Monad OddC where
    return = pure

    (>>=) x f = concatOC (fmap f x)