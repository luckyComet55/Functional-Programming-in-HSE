data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

concat3OC :: OddC a -> OddC a -> OddC a -> OddC a
concat3OC (Un x) (Un y) t = Bi x y t
concat3OC (Bi x y z) a t = Bi x y $ concat3OC z a t
concat3OC (Un x) (Bi y z e) t = Bi x y $ concat3OC (Un z) e t
                                