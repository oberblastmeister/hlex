{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE RoleAnnotations #-}

module Text.HLex.Internal.RangeMap
  ( RangeMap (ranges),
    new,
    unsafeNew,
    union,
    singleton,
    point,
    elems,
  )
where

import Data.List qualified as List
import GHC.Exts (IsList (..))
import Text.HLex.Internal.Range (Range)
import Text.HLex.Internal.Range qualified as Range

newtype RangeMap a m = RangeMap {ranges :: [(Range a, m)]}
  deriving (Show, Eq) via [(Range a, m)]
  deriving (Functor, Foldable, Traversable)

type role RangeMap nominal representational

instance (Range.DiscreteOrdered a, Semigroup m) => IsList (RangeMap a m) where
  type Item (RangeMap a m) = (Range a, m)
  fromList = new
  toList = ranges

new :: (Range.DiscreteOrdered a, Semigroup m) => [(Range a, m)] -> RangeMap a m
new = RangeMap . normalize

unsafeNew :: [(Range a, m)] -> RangeMap a m
unsafeNew = RangeMap

singleton :: Range a -> m -> RangeMap a m
singleton r m = RangeMap [(r, m)]

point :: a -> m -> RangeMap a m
point x m = singleton (Range.point x) m

union :: (Range.DiscreteOrdered a, Semigroup m) => RangeMap a m -> RangeMap a m -> RangeMap a m
union (RangeMap rs1) (RangeMap rs2) = RangeMap $ unionRanges $ merge rs1 rs2
  where
    merge ts1 [] = ts1
    merge [] ts2 = ts2
    merge ts1@(t1 : ts1') ts2@(t2 : ts2') =
      if fst t1 < fst t2
        then t1 : merge ts1' ts2
        else t2 : merge ts1 ts2'

normalize :: (Range.DiscreteOrdered a, Semigroup m) => [(Range a, m)] -> [(Range a, m)]
normalize = unionRanges . List.sortOn fst

unionRanges :: (Range.DiscreteOrdered a, Semigroup m) => [(Range a, m)] -> [(Range a, m)]
unionRanges (t1@(r1, m1) : t2@(r2, m2) : ts) = case Range.union r1 r2 of
  Nothing -> t1 : unionRanges (t2 : ts)
  Just r3 -> let !m3 = m1 <> m2 in unionRanges ((r3, m3) : ts)
unionRanges rs = rs

elems :: Enum a => RangeMap a m -> [(a, m)]
elems (RangeMap rs) = [(x, m) | (r, m) <- rs, x <- Range.elems r]
{-# INLINE elems #-}
