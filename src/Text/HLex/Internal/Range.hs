{-# LANGUAGE RoleAnnotations #-}

module Text.HLex.Internal.Range
  ( Range,
    new,
    point,
    DiscreteOrdered (..),
    isContiguous,
    union,
    pattern RangeV,
    fromTuple,
    toTuple,
    zip,
    elems,
  )
where

import Data.Hashable (Hashable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Prelude hiding (zip)
import Prelude qualified

-- a range that is inclusive at the start and end
data Range a = Range
  { start :: !a,
    end :: !a
  }
  deriving (Show, Eq, Ord, Functor, Generic)

type role Range nominal

instance Hashable a => Hashable (Range a)

pattern RangeV :: a -> a -> Range a
pattern RangeV start end <- Range {start, end}

{-# COMPLETE RangeV #-}

class Ord a => DiscreteOrdered a where
  adjacent :: a -> a -> Bool

instance DiscreteOrdered Int where
  adjacent = boundedAdjacent

instance Ord a => DiscreteOrdered [a] where
  adjacent _ _ = False

new :: (Ord a, HasCallStack) => a -> a -> Range a
new start end
  | start > end = error "start is greater than end"
  | otherwise = Range {start, end}

point :: a -> Range a
point x = Range {start = x, end = x}

-- | Check adjacency, allowing for case where x = maxBound.  Use as the
-- definition of "adjacent" for bounded enumerated types such as Int and Char.
boundedAdjacent :: (Ord a, Enum a) => a -> a -> Bool
boundedAdjacent x y = if x < y then succ x == y else False

isContiguous :: DiscreteOrdered a => Range a -> Range a -> Bool
isContiguous Range {start = start1, end = end1} Range {start = start2, end = end2} =
  (max start1 start2) <= ((min end1 end2)) || adjacent end1 start2

union :: DiscreteOrdered a => Range a -> Range a -> Maybe (Range a)
union r1@Range {start = start1, end = end1} r2@Range {start = start2, end = end2}
  | isContiguous r1 r2 = Just Range {start, end}
  | otherwise = Nothing
  where
    start = min start1 start2
    end = max end1 end2

fromTuple :: (a, a) -> Range a
fromTuple (start, end) = Range {start, end}

toTuple :: Range a -> (a, a)
toTuple (RangeV start end) = (start, end)

zip :: Range [Word8] -> [Range Word8]
zip (RangeV start end) = fromTuple <$> Prelude.zip start end

elems :: Enum a => Range a -> [a]
elems (RangeV start end) = [start .. end]
{-# INLINE elems #-}
