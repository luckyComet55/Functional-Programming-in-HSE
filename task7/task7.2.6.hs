newtype Arr2 e1 e2 a = Arr2 { getArr2 :: e1 -> e2 -> a }
newtype Arr3 e1 e2 e3 a = Arr3 { getArr3 :: e1 -> e2 -> e3 -> a }

instance Functor (Arr2 e1 e2) where
  fmap f (Arr2 a) = Arr2 (\e1 e2 -> f (a e1 e2))

instance Functor (Arr3 e1 e2 e3) where
  fmap f (Arr3 a) = Arr3 (\e1 e2 e3 -> f (a e1 e2 e3))

instance Applicative (Arr2 e1 e2) where
  pure a = Arr2 (\e1 e2 -> a)
  (<*>) (Arr2 f) (Arr2 g) = Arr2 (\e1 e2 -> f e1 e2 (g e1 e2))

instance Applicative (Arr3 e1 e2 e3) where
  pure a = Arr3 (\e1 e2 e3 -> a)
  (<*>) (Arr3 f) (Arr3 g) = Arr3 (\e1 e2 e3 -> f e1 e2 e3 (g e1 e2 e3))