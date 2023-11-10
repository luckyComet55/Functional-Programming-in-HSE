{-# LANGUAGE InstanceSigs #-}

data E l r = L l | R r
    deriving (Eq, Show)

instance Functor (E l) where
  fmap :: (a -> b) -> E l a -> E l b
  fmap = undefined

instance Applicative (E l) where
  pure :: a -> E l a
  pure  = undefined
  (<*>) :: E l (a -> b) -> E l a -> E l b
  (<*>) = undefined