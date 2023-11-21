data Tree a = Leaf | Node (Tree a) a (Tree a) 
  deriving Show

treeSum :: Tree Integer -> Integer
treeSum Leaf = 0
treeSum (Node tl a tr) = a + treeSum tl + treeSum tr

treeHeight :: Tree a -> Int
treeHeight Leaf = 0
treeHeight (Node tl a tr) = 1 + max (treeHeight tl) (treeHeight tr)