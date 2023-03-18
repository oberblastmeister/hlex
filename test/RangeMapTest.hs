{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module RangeMapTest where

import Data.Bifunctor (second)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import Data.Maybe (fromMaybe)
import GHC.Exts (toList)
import RangeTest ()
import Text.HLex.Internal.Range (Range)
import Text.HLex.Internal.Range qualified as Range
import Text.HLex.Internal.RangeMap
import Text.HLex.Internal.RangeMap qualified as RangeMap

-- instance Arbitrary a => Arbitrary (RangeMap a) where
--   arbitrary = smaller 5 $ do
--     ns1 <- arbitrary
--     ns2 <- arbitrary
--     let rs =
--           toRanges
--             . map List.head
--             . List.group
--             . List.sort
--             $ ns1 ++ concatMap (\n -> [n, n]) (ns2 :: [Int])
--     RangeMap . IntMap.fromList
--       <$> mapM
--         ( \r -> do
--             m <- arbitrary
--             pure $ (Range.start r, Range.end r :!: m)
--         )
--         rs
--     where
--       toRanges (f : t : rs) = Range.unsafeNew f t : toRanges rs
--       toRanges _ = []

--   shrink f =
--     [ RangeMap $ IntMap.delete i map
--       | (i, _) <- toList map
--     ]
--       ++ [ RangeMap $ IntMap.insert i (p :!: x) map
--            | (i, p :!: x) <- toList map,
--              x <- shrink x
--          ]
--     where
--       map = rangeMap f

-- instance CoArbitrary a => CoArbitrary (RangeMap a) where
--   coarbitrary (RangeMap f) =
--     coarbitrary $
--       fmap (second (\(x :!: y) -> (x, y))) $
--         toList f

-- smaller :: Int -> Gen a -> Gen a
-- smaller i = scale $ \n -> 1 + n `div` i

-- type RangeMap' = RangeMap [Int]

-- prop_RangeMapInvariant1 :: RangeMap' -> Bool
-- prop_RangeMapInvariant1 = RangeMap.invariant

-- prop_rangeMapInvariant2 :: RangeMap' -> Bool
-- prop_rangeMapInvariant2 = all RangeMap.invariant . shrink

-- -- prop_rangeMapInvariant3 :: [Range] -> [Int] -> Bool
-- -- prop_rangeMapInvariant3 rs m =
-- --   rangeMapInvariant $ several (take 5 rs) m

-- prop_insertWith ::
--   (Fun ([Int], [Int]) [Int]) ->
--   Range ->
--   [Int] ->
--   RangeMap' ->
--   Property
-- prop_insertWith (Fun _ g) = propInsertWith' $ curry g

-- propInsertWith' ::
--   ([Int] -> [Int] -> [Int]) ->
--   Range ->
--   [Int] ->
--   RangeMap' ->
--   Property
-- propInsertWith' g r m f =
--   overlaps (Just r) (RangeMap.coveringRange f) $ expected === actual
--   where
--     expected =
--       IntMap.unionWith
--         g
--         (IntMap.fromList [(p, m) | p <- Range.elems r])
--         (RangeMap.toMap f)
--     actual = RangeMap.toMap (RangeMap.insertWith g r m f)

-- overlaps :: Testable p => Maybe Range -> Maybe Range -> p -> Property
-- overlaps r1 r2 =
--   classify
--     (fromMaybe False $ Range.overlaps <$> r1 <*> r2)
--     "overlapping"

-- -- bounds :: (Int, Int)
-- -- bounds = (1, 100)

-- -- genRange :: QuickCheck.Gen Range
-- -- genRange = do
-- --   x <- QuickCheck.choose bounds
-- --   y <- QuickCheck.choose bounds
-- --   pure $ Range.new (min x y) (max x y)

-- -- spec :: Spec
-- -- spec = do
-- --   prop "RangeMap invariant" $ do
-- --     rm <- fromList <$> QuickCheck.listOf ((,()) <$> genRange)
-- --     let !_ = traceId $ "rm: " ++ show rm
-- --     pure $ RangeMap.invariant rm
