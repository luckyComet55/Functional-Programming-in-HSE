data OddC a = Un a | Bi a a (OddC a) deriving (Eq, Show)

concatOC :: OddC (OddC a) -> OddC a
concatOC (Un a) = a
concatOC (Bi (Un x) (Un y) z) = Bi x y (concatOC z)
concatOC (Bi (Un x) (Bi y z a) k) = Bi x y (concatOC (Bi (Un z) a k))
concatOC (Bi (Bi x y z) e k) = Bi x y (concatOC (Bi z e k))