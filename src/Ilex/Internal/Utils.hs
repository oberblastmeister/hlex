module Ilex.Internal.Utils where

import Data.Bifunctor (Bifunctor, bimap)
import Data.RangeSet.List (RSet)
import Data.RangeSet.List qualified as RSet
import Data.Word (Word8)
import Numeric.Interval.NonEmpty (Interval, (...))
import Numeric.Interval.NonEmpty qualified as I

type Byte = Word8

type ByteSet = RSet Byte

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f
{-# INLINE both #-}

intervalListToRangeSet :: (Ord a, Enum a) => [Interval a] -> RSet a
intervalListToRangeSet = RSet.fromRangeList . fmap (\i -> (I.inf i, I.sup i))

rangeSetToIntervalList :: (Ord a) => RSet a -> [Interval a]
rangeSetToIntervalList = fmap (uncurry (...)) . RSet.toRangeList
