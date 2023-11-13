data Tree a = Nil | Branch (Tree a) a (Tree a)  deriving (Eq, Show)

newtype Preorder a = PreO (Tree a) deriving (Eq, Show)
newtype Postorder a = PostO (Tree a) deriving (Eq, Show)
newtype Levelorder a = LevelO (Tree a) deriving (Eq, Show)

-- in-order foldr for tree
instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ z Nil = z
    foldr f z (Branch tl a tr) = foldr f res tl
        where
            res = f a res'
            res' = foldr f z tr

instance Foldable Preorder where
    foldr :: (a -> b -> b) -> b -> Preorder a -> b
    foldr _ z (PreO Nil) = z
    foldr f z (PreO (Branch tl a tr)) = f a res
        where
            res = foldr f res' (PreO tl)
            res' = foldr f z (PreO tr)

instance Foldable Postorder where
    foldr :: (a -> b -> b) -> b -> Postorder a -> b
    foldr _ z (PostO Nil) = z
    foldr f z (PostO (Branch tl a tr)) = foldr f res (PostO tl)
        where
            res = foldr f res' (PostO tr)
            res' = f a z

instance Foldable Levelorder where
    foldr :: (a -> b -> b) -> b -> Levelorder a -> b
    foldr _ z (LevelO Nil) = z
    foldr f z (LevelO tree@(Branch tl a tr)) = helper [tree] []
        where
            helper [] rest
                | null rest = z
                | otherwise = helper (reverse rest) []
            helper (Nil : xs) rest = helper xs rest
            helper ((Branch tl a tr) : xs) rest = f a (helper xs (tr : tl : rest))