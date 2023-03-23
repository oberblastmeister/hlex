{-# LANGUAGE TemplateHaskell #-}

module Utf8Test (tests) where

import Data.Bits (unsafeShiftR, (.&.), (.|.))
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.Maybe qualified as Maybe
import Data.Word (Word8)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Numeric.Interval.NonEmpty (Interval, (...))
import Numeric.Interval.NonEmpty qualified as I
import Test.Tasty
import Test.Tasty.Hedgehog
import Text.HLex.Internal.Utf8

genInterval :: Ord a => Gen a -> Gen (Interval a)
genInterval gen = (...) <$> gen <*> gen

intervalToRange :: Interval a -> Range a
intervalToRange i = Range.constant (I.inf i) (I.sup i)

genFromInterval :: Enum a => Interval a -> Gen a
genFromInterval i = Gen.enum (I.inf i) (I.sup i)

-- prop_never_accepts_surrogate_codepoints :: Property
-- prop_never_accepts_surrogate_codepoints = property do
--   cp <- forAll $ genInterval $ Char.ord <$> Gen.unicodeAll
--   let sequences = utf8Sequences cp
--   for_ [0xd800 :: Int .. 0xdfff] \cp -> do
--     let !bs = Maybe.fromJust $ encodeSurrogate cp
--     assert $ not $ any (`matchUtf8Sequence` bs) sequences

prop_single_codepoint_one_sequence :: Property
prop_single_codepoint_one_sequence = property do
  cp <- forAll $ Gen.filter (`I.notMember` surrogateRange) $ Char.ord <$> Gen.unicodeAll
  let sequences = utf8Sequences $ I.singleton cp
  length sequences === 1

prop_no_sequence_for_surrogate_codepoints :: Property
prop_no_sequence_for_surrogate_codepoints = property do
  cp <- forAll $ genInterval $ genFromInterval surrogateRange
  let sequences = utf8Sequences cp
  length sequences === 0

prop_ascii_range_same :: Property
prop_ascii_range_same = property do
  i <- forAll $ genInterval $ Char.ord <$> Gen.ascii
  let toB = fromIntegral @Int @Word8
  utf8Sequences i === [UOne $ toB (I.inf i) ... toB (I.sup i)]

encodeSurrogate :: Int -> Maybe [Word8]
encodeSurrogate cp
  | cp < 0xd800 || cp > 0xdfff = Nothing
  | otherwise =
      Just
        [ toB ((cp `unsafeShiftR` 12) .&. 0x0f) .|. tagThreeB,
          toB ((cp `unsafeShiftR` 6) .&. 0x3f) .|. tagCont,
          toB (cp .&. 0x3f) .|. tagCont
        ]
  where
    toB = fromIntegral @Int @Word8
    tagCont = 0b1000_0000
    tagThreeB = 0b1110_0000

tests :: TestTree
tests =
  testGroup
    "Utf8RangeTest"
    [ fromGroup $$(discover),
      testProperty "multilingual plane" $ property do
        utf8Sequences (0x00 ... 0xffff)
          === [ UOne (0x0 ... 0x7f),
                UTwo (0xc2 ... 0xdf) (0x80 ... 0xbf),
                UThree (0xe0 ... 0xe0) (0xa0 ... 0xbf) (0x80 ... 0xbf),
                UThree (0xe1 ... 0xec) (0x80 ... 0xbf) (0x80 ... 0xbf),
                UThree (0xed ... 0xed) (0x80 ... 0x9f) (0x80 ... 0xbf),
                UThree (0xee ... 0xef) (0x80 ... 0xbf) (0x80 ... 0xbf)
              ],
      testProperty "all" $ property do
        utf8Sequences (0x00 ... 0x10ffff)
          === [ UOne (0x0 ... 0x7f),
                UTwo (0xc2 ... 0xdf) (0x80 ... 0xbf),
                UThree (0xe0 ... 0xe0) (0xa0 ... 0xbf) (0x80 ... 0xbf),
                UThree (0xe1 ... 0xec) (0x80 ... 0xbf) (0x80 ... 0xbf),
                UThree (0xed ... 0xed) (0x80 ... 0x9f) (0x80 ... 0xbf),
                UThree (0xee ... 0xef) (0x80 ... 0xbf) (0x80 ... 0xbf),
                UFour (0xf0 ... 0xf0) (0x90 ... 0xbf) (0x80 ... 0xbf) (0x80 ... 0xbf),
                UFour (0xf1 ... 0xf3) (0x80 ... 0xbf) (0x80 ... 0xbf) (0x80 ... 0xbf),
                UFour (0xf4 ... 0xf4) (0x80 ... 0x8f) (0x80 ... 0xbf) (0x80 ... 0xbf)
              ]
    ]
