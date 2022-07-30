{-# OPTIONS_GHC -Wno-orphans #-}

module RangeMapTest where

import Data.Bifunctor (second)
import Data.IntMap.Strict qualified as IntMap
import Data.List qualified as List
import GHC.Exts (toList)
import Test.QuickCheck (Arbitrary, CoArbitrary, arbitrary, coarbitrary, shrink)
import Test.QuickCheck qualified as QuickCheck
import Text.HLex.Internal.Range qualified as Range
import Text.HLex.Internal.RangeMap
import Text.HLex.Internal.RangeMap qualified as RangeMap

instance Arbitrary a => Arbitrary (RangeMap a) where
  arbitrary = smaller 5 $ do
    rs <-
      ( \ns1 ns2 ->
          toRanges $
            List.sort $
              ns1 ++ concatMap (\n -> [n, n]) (ns2 :: [Int])
        )
        <$> arbitrary <*> arbitrary
    RangeMap . IntMap.fromList
      <$> mapM
        (\r -> (\m -> (Range.start r, Range.end r :!: m)) <$> arbitrary)
        rs
    where
      toRanges (f : t : rs)
        | f == t = toRanges (t : rs)
        | otherwise =
            Range.unsafeNew f t :
            toRanges
              ( case rs of
                  f : rs | t == f -> rs
                  _ -> rs
              )
      toRanges _ = []

  shrink f =
    [ RangeMap $ IntMap.delete i map
      | (i, _) <- toList map
    ]
      ++ [ RangeMap $ IntMap.insert i (p :!: x) map
           | (i, p :!: x) <- toList map,
             x <- shrink x
         ]
    where
      map = rangeMap f

instance CoArbitrary a => CoArbitrary (RangeMap a) where
  coarbitrary (RangeMap f) =
    coarbitrary $
      fmap (second (\(x :!: y) -> (x, y))) $
        toList f

smaller :: Int -> QuickCheck.Gen a -> QuickCheck.Gen a
smaller i = QuickCheck.scale $ \n -> 1 + n `div` i

type RangeMap' = RangeMap [Int]

prop_RangeMapInvariant1 :: RangeMap' -> Bool
prop_RangeMapInvariant1 = RangeMap.invariant

prop_rangeMapInvariant2 :: RangeMap' -> Bool
prop_rangeMapInvariant2 = all RangeMap.invariant . shrink

-- bounds :: (Int, Int)
-- bounds = (1, 100)

-- genRange :: QuickCheck.Gen Range
-- genRange = do
--   x <- QuickCheck.choose bounds
--   y <- QuickCheck.choose bounds
--   pure $ Range.new (min x y) (max x y)

-- spec :: Spec
-- spec = do
--   prop "RangeMap invariant" $ do
--     rm <- fromList <$> QuickCheck.listOf ((,()) <$> genRange)
--     let !_ = traceId $ "rm: " ++ show rm
--     pure $ RangeMap.invariant rm
