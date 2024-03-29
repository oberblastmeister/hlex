{-# LANGUAGE UndecidableInstances #-}

module Hlex.Internal.CharSet
  ( CharSet (..),
    empty,
    singleton,
    insert,
    union,
    difference,
    intersection,
    toList,
    toRangeList,
    fromList,
    fromPred,
    null,
    singletonRange,
    (\\),
  )
where

import Data.RangeSet.List (RSet)
import Data.RangeSet.List qualified as RSet
import GHC.Exts (IsList (Item))
import GHC.Exts qualified
import Prelude hiding (null)

newtype CharSet = CharSet {unCharSet :: RSet Char}
  deriving (Show, Eq, Ord, Semigroup, Monoid) via RSet Char

instance IsList CharSet where
  type Item CharSet = Char
  fromList = CharSet . RSet.fromList
  toList = RSet.toList . unCharSet

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

fromList :: [Char] -> CharSet
fromList = CharSet . RSet.fromList

fromPred :: (Char -> Bool) -> CharSet
fromPred f = CharSet . RSet.fromAscList . filter f $ [minBound .. maxBound]
