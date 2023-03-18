module Text.HLex.Internal.CharSet
  ( CharSet,
    empty,
    singleton,
    insert,
    union,
    difference,
    intersection,
    toList,
    toRangeList,
  )
where

import Data.RangeSet.List (RSet)
import Data.RangeSet.List qualified as RSet

newtype CharSet = CharSet {unCharSet :: RSet Char}
  deriving (Show, Eq, Ord, Semigroup, Monoid) via RSet Char

empty :: CharSet
empty = CharSet RSet.empty

singleton :: Char -> CharSet
singleton = CharSet . RSet.singleton

insert :: Char -> CharSet -> CharSet
insert x (CharSet y) = CharSet $ RSet.insert x y

union :: CharSet -> CharSet -> CharSet
union (CharSet x) (CharSet y) = CharSet $ RSet.union x y

difference :: CharSet -> CharSet -> CharSet
difference (CharSet x) (CharSet y) = CharSet $ RSet.difference x y

intersection :: CharSet -> CharSet -> CharSet
intersection (CharSet x) (CharSet y) = CharSet $ RSet.intersection x y

toList :: CharSet -> [Char]
toList = RSet.toList . unCharSet

toRangeList :: CharSet -> [(Char, Char)]
toRangeList = RSet.toRangeList . unCharSet
