module Ilex.Internal.CharSet
  ( CharSet (..),
    empty,
    singleton,
    insert,
    union,
    difference,
    intersection,
    toList,
    toRangeList,
    fromString,
    fromPred,
    null,
    singletonRange,
    (\\),
  )
where

import Data.RangeSet.List (RSet)
import Data.RangeSet.List qualified as RSet
import Prelude hiding (null)

newtype CharSet = CharSet {unCharSet :: RSet Char}
  deriving (Show, Eq, Ord, Semigroup, Monoid) via RSet Char

empty :: CharSet
empty = CharSet RSet.empty

null :: CharSet -> Bool
null = RSet.null . unCharSet

singleton :: Char -> CharSet
singleton = CharSet . RSet.singleton

singletonRange :: (Char, Char) -> CharSet
singletonRange = CharSet . RSet.singletonRange

insert :: Char -> CharSet -> CharSet
insert x (CharSet y) = CharSet $ RSet.insert x y

union :: CharSet -> CharSet -> CharSet
union (CharSet x) (CharSet y) = CharSet $ RSet.union x y

difference :: CharSet -> CharSet -> CharSet
difference (CharSet x) (CharSet y) = CharSet $ RSet.difference x y

intersection :: CharSet -> CharSet -> CharSet
intersection (CharSet x) (CharSet y) = CharSet $ RSet.intersection x y

(\\) :: CharSet -> CharSet -> CharSet
(\\) = difference

toList :: CharSet -> [Char]
toList = RSet.toList . unCharSet

toRangeList :: CharSet -> [(Char, Char)]
toRangeList = RSet.toRangeList . unCharSet

fromString :: [Char] -> CharSet
fromString = CharSet . RSet.fromList

fromPred :: (Char -> Bool) -> CharSet
fromPred f = CharSet . RSet.fromAscList . filter f $ [minBound .. maxBound]
