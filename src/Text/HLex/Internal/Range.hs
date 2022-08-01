{-# LANGUAGE RoleAnnotations #-}

module Text.HLex.Internal.Range where

-- ( Range (start, end),
--   new,
--   elems,
--   -- point,
--   -- DiscreteOrdered (..),
--   -- isContiguous,
--   -- union,
--   pattern RangeV,
--   fromTuple,
--   toTuple,
--   -- -- zip,
--   -- elems,
--   unsafeNew,
--   unsafeMapBoth,
--   point,
--   invariant,
--   overlaps,
-- )

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Prelude hiding (null, zip)

-- a range that is inclusive at the start and inclusive at end
data Range = Range
  { start :: !Int,
    end :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

instance Hashable Range

pattern RangeV :: Int -> Int -> Range
pattern RangeV start end <- Range {start, end}

{-# COMPLETE RangeV #-}

class Ord a => DiscreteOrdered a where
  adjacent :: a -> a -> Bool

instance DiscreteOrdered Int where
  adjacent = boundedAdjacent

instance Ord a => DiscreteOrdered [a] where
  adjacent _ _ = False

new :: HasCallStack => Int -> Int -> Range
new start end
  | start > end = error "start is greater than end"
  | otherwise = Range {start, end}

unsafeNew :: Int -> Int -> Range
unsafeNew start end = Range {start, end}

unsafeMapBoth :: (Int -> Int) -> Range -> Range
unsafeMapBoth f Range {start, end} = Range {start = f start, end = f end}

point :: Int -> Range
point x = Range {start = x, end = x}

-- | Check adjacency, allowing for case where x = maxBound.  Use as the
-- definition of "adjacent" for bounded enumerated types such as Int and Char.
boundedAdjacent :: (Ord a, Enum a) => a -> a -> Bool
boundedAdjacent x y = if x < y then succ x == y else False

isContiguous :: Range -> Range -> Bool
isContiguous Range {start = start1, end = end1} Range {start = start2, end = end2} =
  (max start1 start2) <= ((min end1 end2)) || adjacent end1 start2

union :: Range -> Range -> Maybe Range
union r1@Range {start = start1, end = end1} r2@Range {start = start2, end = end2}
  | isContiguous r1 r2 = Just Range {start, end}
  | otherwise = Nothing
  where
    start = min start1 start2
    end = max end1 end2

intersection :: Range -> Range -> Maybe Range
intersection (Range s1 e1) (Range s2 e2) =
  if s3 <= e3
    then Just $ Range s3 e3
    else Nothing
  where
    s3 = max s1 s2
    e3 = min e1 e2

fromTuple :: (Int, Int) -> Range
fromTuple (start, end) = Range {start, end}

toTuple :: Range -> (Int, Int)
toTuple (RangeV start end) = (start, end)

-- -- zip :: Range [Word8] -> [Range Word8]
-- -- zip (RangeV start end) = fromTuple <$> Prelude.zip start end

elems :: Range -> [Int]
elems (RangeV start end) = [start .. end]
{-# INLINE elems #-}

invariant :: Range -> Bool
invariant Range {start, end} = start <= end

overlaps :: Range -> Range -> Bool
overlaps (Range s1 e1) (Range s2 e2) = max s1 s2 >= min e1 e2
