{-# LANGUAGE InstanceSigs #-}

data E l r = L l | R r
    deriving (Eq, Show)

instance Functor (E l) where
  fmap :: (a -> b) -> E l a -> E l b
  fmap _ (L x) = L x
  fmap g (R y) = R (g y)

instance Applicative (E l) where
  pure :: a -> E l a
  pure = R
  (<*>) :: E l (a -> b) -> E l a -> E l b
  (<*>) (L x) _ = L x
  (<*>) (R y) f = fmap y f