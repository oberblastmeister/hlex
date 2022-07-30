{-# LANGUAGE CPP #-}
{-# LANGUAGE FieldSelectors #-}
{-# LANGUAGE RoleAnnotations #-}

module Text.HLex.Internal.RangeMap where

-- ( RangeMap (rangeMap),
-- -- new,
-- -- unsafeNew,
-- -- union,
-- -- singleton,
-- -- point,
-- -- elems,
-- )

import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import GHC.Exts (IsList (..))
import Text.HLex.Internal.Range (Range)
import Text.HLex.Internal.Range qualified as Range

newtype RangeMap a = RangeMap {rangeMap :: IntMap (Pair a)}
  deriving (Show, Eq) via (IntMap (Pair a))
  deriving (Functor, Foldable, Traversable)

data Pair a = !Int :!: !a
  deriving (Show, Eq, Functor, Foldable, Traversable)

insert :: Semigroup a => Range -> a -> RangeMap a -> RangeMap a
insert = insertWith (<>)

insertWith :: (a -> a -> a) -> Range -> a -> RangeMap a -> RangeMap a
insertWith combine r m (RangeMap f) = case equal of
  Just ((p :!: m')) ->
    case compare (Range.end r) p of
      EQ ->
        -- The range r matches exactly.
        RangeMap $
          IntMap.insert (Range.start r) (p :!: combine m m') f
      LT ->
        -- The range r is strictly shorter.
        RangeMap $
          IntMap.insert (Range.end r + 1) (p :!: m') $
            IntMap.insert (Range.start r) (Range.end r :!: combine m m') f
      GT ->
        -- The range r is strictly longer. Continue recursively.
        insertWith combine (Range.unsafeNew (p + 1) (Range.end r)) m $
          RangeMap $ IntMap.insert (Range.start r) (p :!: combine m m') f
  Nothing ->
    -- Find the part of r that does not overlap with anything in
    -- smaller or larger, if any.
    case (overlapLeft, overlapRight) of
      (Nothing, Nothing) ->
        -- No overlap.
        RangeMap $ IntMap.insert (Range.start r) (Range.end r :!: m) f
      (Nothing, Just p) ->
        -- Overlap on the right. Continue recursively.
        insertWith combine (Range.unsafeNew p (Range.end r)) m $
          -- we can subtract 1 because p is always greater than (Range.start r)
          RangeMap $ IntMap.insert (Range.start r) ((p - 1) :!: m) f
      (Just (p1, (p2 :!: m')), overlapRight) -> case overlapRight of
        -- this needs some work
        Just p3 ->
          -- Overlap on both sides. Continue recursively.
          insertWith combine (Range.unsafeNew p3 (Range.end r)) m $
            RangeMap $
              ( if p2 + 1 > p3 - 1
                  then -- The left range ends exactly where the right range
                  -- starts.
                    id
                  else -- There is something between the left and right
                  -- ranges.
                    IntMap.insert (p2 + 1) ((p3 - 1) :!: m)
              )
                $ IntMap.insert (Range.start r) (p2 :!: (combine m m')) $ afterNonOverlappedLeft
        Nothing ->
          case compare p2 (Range.end r) of
            LT ->
              -- Overlap on the left, the left range ends before r ends.
              RangeMap $
                -- we can add 1 here because the p2 is always less than (Range.end r)
                IntMap.insert (p2 + 1) (Range.end r :!: m) $
                  -- inserting the overlapping part, ending where the left range ends
                  IntMap.insert (Range.start r) (p2 :!: (combine m m')) $ afterNonOverlappedLeft
            EQ ->
              -- Overlap on the left, the left range ends where r
              -- ends.
              RangeMap $
                -- insert the overlapping part, ending where the range ends
                IntMap.insert (Range.start r) (Range.end r :!: (combine m m')) $ afterNonOverlappedLeft
            GT ->
              -- Overlap on the left, the left range ends after r.
              RangeMap $
                -- insert the non overlapping part after the range ends
                -- we can add 1 here because p2 is always greater than r
                IntMap.insert (Range.end r + 1) (p2 :!: m') $
                  -- insert the overlapping part, ending where the range ends
                  IntMap.insert (Range.start r) (Range.end r :!: combine m m') $ afterNonOverlappedLeft
        where
          afterNonOverlappedLeft =
            ( if p1 == Range.start r
                then id
                else IntMap.insert p1 ((Range.start r - 1) :!: m')
            )
              f
  where
    (smaller, equal, larger) = IntMap.splitLookup (Range.start r) f

    overlapRight = case IntMap.lookupMin larger of
      Nothing -> Nothing
      Just (start, _)
        | start <= Range.end r -> Just start
        | otherwise -> Nothing

    overlapLeft = case IntMap.lookupMax smaller of
      Nothing -> Nothing
      Just s@(_, (end :!: _))
        | Range.start r <= end -> Just s
        | otherwise -> Nothing

-- | Invariant for 'RangeMap'.
--
--  The ranges must not overlap.
invariant :: Semigroup a => RangeMap a -> Bool
invariant f =
  and
    [ all Range.invariant rs,
      case rs of
        [] -> True
        r : rs -> and $ zipWith (<=) (map Range.end $ init1 r rs) (map Range.start rs)
    ]
  where
    rs = map fst $ toList f

-- | 'init' of non-empty list, safe.
--   O(n).
--   @init1 a as = init (a:as)@
init1 :: a -> [a] -> [a]
init1 a = \case
  [] -> []
  b : bs -> a : init1 b bs

empty :: RangeMap a
empty = RangeMap mempty

instance Semigroup m => IsList (RangeMap m) where
  type Item (RangeMap m) = (Range, m)
  fromList = foldl' (flip $ uncurry insert) empty
  toList =
    fmap (\(start, (end :!: m)) -> (Range.unsafeNew start end, m))
      . toList
      . rangeMap

elems :: RangeMap a -> [(Int, a)]
elems (RangeMap {rangeMap}) =
  [ (i, m)
    | (start, end :!: m) <- toList rangeMap,
      i <- Range.elems $ Range.unsafeNew start end
  ]
{-# INLINE elems #-}

-- new :: (Semigroup m) => [(Range , m)] -> RangeMap m
-- new = RangeMap . normalize

-- unsafeNew :: [(Range , m)] -> RangeMap m
-- unsafeNew = RangeMap

-- singleton :: Range -> m -> RangeMap a
-- singleton r m = RangeMap [(r, m)]

-- point :: a -> m -> RangeMap a
-- point x m = singleton (Range.point x) m

-- union :: (Range.DiscreteOrdered a, Semigroup m) => RangeMap a -> RangeMap a -> RangeMap a
-- union (RangeMap rs1) (RangeMap rs2) = RangeMap $ unionRanges $ merge rs1 rs2
--   where
--     merge ts1 [] = ts1
--     merge [] ts2 = ts2
--     merge ts1@(t1 : ts1') ts2@(t2 : ts2') =
--       if fst t1 < fst t2
--         then t1 : merge ts1' ts2
--         else t2 : merge ts1 ts2'

-- normalize :: (Range.DiscreteOrdered a, Semigroup m) => [(Range, m)] -> [(Range, m)]
-- normalize = unionRanges . List.sortOn fst

-- unionRanges :: (Range.DiscreteOrdered a, Semigroup m) => [(Range, m)] -> [(Range, m)]
-- unionRanges (t1@(r1, m1) : t2@(r2, m2) : ts) = case Range.union r1 r2 of
--   Nothing -> t1 : unionRanges (t2 : ts)
--   Just r3 -> let !m3 = m1 <> m2 in unionRanges ((r3, m3) : ts)
-- unionRanges rs = rs

-- elems :: Enum a => RangeMap a -> [(a, m)]
-- elems (RangeMap rs) = [(x, m) | (r, m) <- rs, x <- Range.elems r]
-- {-# INLINE elems #-}
