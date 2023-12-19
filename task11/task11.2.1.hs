{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
import Control.Monad.Except (MonadError)
import Control.Monad.Error (MonadError(throwError))

data ListIndexError
  = ErrTooLargeIndex Int
  | ErrNegativeIndex
  | OtherErr String
  deriving (Eq, Show)

helper :: Int -> [a] -> Bool
helper 0 _ = True
helper _ [] = False
helper n (_ : ys) = helper (n - 1) ys

infixl 9 !!!

(!!!) :: MonadError ListIndexError m => [a] -> Int -> m a
xs !!! n    | n < 0              = throwError ErrNegativeIndex
            | helper (n + 1) xs  = return (xs !! n)
            | otherwise          = throwError $ ErrTooLargeIndex n