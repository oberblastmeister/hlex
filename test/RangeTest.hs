{-# OPTIONS_GHC -Wno-orphans #-}

module RangeTest where

import Test.QuickCheck
import Text.HLex.Internal.Range

instance Arbitrary Range where
  arbitrary = do
    p1 <- arbitrary
    p2 <- arbitrary
    pure $ Range (min p1 p2) (max p1 p2)
