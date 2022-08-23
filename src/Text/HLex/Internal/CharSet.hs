module Text.HLex.Internal.CharSet
  ( RangeSet (ranges),
  -- unsafeNew,
  -- union,
  -- singleton,
  -- point,
  )
where

import Data.List qualified as List
import GHC.Exts (IsList (..))

data Range = Range
  { start :: !Char,
    end :: !Char
  }

newtype RangeSet = RangeSet {ranges :: [Range]}

-- deriving (Show, Eq)

-- instance IsList (RangeSet) where
--   type Item RangeSet = Range
--   fromList = RangeSet . normalize
--   toList = ranges

singleton :: Range -> RangeSet
singleton = RangeSet . pure

-- point :: Int -> RangeSet
-- point = singleton . Range.point

-- union :: RangeSet -> RangeSet -> RangeSet
-- union (RangeSet rs1) (RangeSet rs2) = RangeSet $ unionRanges $ merge rs1 rs2
--   where
--     merge rs1 [] = rs1
--     merge [] rs2 = rs2
--     merge rs1@(r1 : rs1') rs2@(r2 : rs2') =
--       if r1 < r2
--         then r1 : merge rs1' rs2
--         else r2 : merge rs1 rs2'

-- unsafeNew :: [Range] -> RangeSet
-- unsafeNew = RangeSet

-- normalize :: [Range] -> [Range]
-- normalize = unionRanges . List.sort

-- unionRanges :: [Range] -> [Range]
-- unionRanges (r1 : r2 : rs) = case Range.union r1 r2 of
--   Nothing -> r1 : unionRanges (r2 : rs)
--   Just r3 -> unionRanges (r3 : rs)
-- unionRanges rs = rs
