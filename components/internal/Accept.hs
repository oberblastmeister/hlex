module Accept (Accept (..)) where

data Accept a = Accept
  { value :: a,
    priority :: !Int
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)
