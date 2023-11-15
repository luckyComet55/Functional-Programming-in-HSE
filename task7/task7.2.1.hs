import Control.Applicative (ZipList(ZipList), getZipList)

infixl 4 >$<, >*<

(>$<) :: (a -> b) -> [a] -> [b]
(>$<) f x = getZipList (f <$> ZipList x)

(>*<) :: [a -> b] -> [a] -> [b]
(>*<) x y = getZipList (ZipList x <*> ZipList y)