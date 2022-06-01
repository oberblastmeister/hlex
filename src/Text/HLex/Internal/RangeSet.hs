{-# LANGUAGE FieldSelectors #-}

module Text.HLex.Internal.RangeSet
  ( RangeSet (ranges),
    new,
    unsafeNew,
    union,
    singleton,
    point,
  )
where

import Data.List qualified as List
import GHC.Exts (IsList (..))
import Text.HLex.Internal.Range (Range)
import Text.HLex.Internal.Range qualified as Range

newtype RangeSet a = RangeSet {ranges :: [Range a]}
  deriving (Show, Eq)

instance Range.DiscreteOrdered a => IsList (RangeSet a) where
  type Item (RangeSet a) = Range a
  fromList = new
  toList = ranges

new :: Range.DiscreteOrdered a => [Range a] -> RangeSet a
new = RangeSet . normalize

singleton :: Range a -> RangeSet a
singleton = RangeSet . pure

point :: a -> RangeSet a
point = singleton . Range.point

union :: Range.DiscreteOrdered a => RangeSet a -> RangeSet a -> RangeSet a
union (RangeSet rs1) (RangeSet rs2) = RangeSet $ unionRanges $ merge rs1 rs2
  where
    merge rs1 [] = rs1
    merge [] rs2 = rs2
    merge rs1@(r1 : rs1') rs2@(r2 : rs2') =
      if r1 < r2
        then r1 : merge rs1' rs2
        else r2 : merge rs1 rs2'

unsafeNew :: [Range a] -> RangeSet a
unsafeNew = RangeSet

normalize :: Range.DiscreteOrdered a => [Range a] -> [Range a]
normalize = unionRanges . List.sort

unionRanges :: Range.DiscreteOrdered a => [Range a] -> [Range a]
unionRanges (r1 : r2 : rs) = case Range.union r1 r2 of
  Nothing -> r1 : unionRanges (r2 : rs)
  Just r3 -> unionRanges (r3 : rs)
unionRanges rs = rs
