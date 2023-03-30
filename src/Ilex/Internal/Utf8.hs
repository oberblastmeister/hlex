{-# LANGUAGE MultiWayIf #-}

module Ilex.Internal.Utf8
  ( encodeCharUtf8,
    encodeCharUtf8WithInvalid,
    encodeScalarValueWithInvalid,
    surrogateRange,
    splitMaxScalarValues,
    splitSurrogate,
    Utf8Amount (..),
    Utf8Range,
    Utf8Sequence,
    matchUtf8Sequence,
    utf8AmountFromList,
    utf8Sequences,
    utf8ScalarRanges,
    toScalarRange,
    validScalarRange,
  )
where

import Control.Applicative (asum)
import Control.Monad ((<=<))
import Data.Bits (complement, shiftR, unsafeShiftL, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Char qualified as Char
import Data.Foldable qualified as Foldable
import Data.Maybe (fromMaybe)
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Word (Word8)
import Numeric.Interval.NonEmpty (Interval, (...))
import Numeric.Interval.NonEmpty qualified as I

type ScalarRange = Interval Int

type Utf8Range = Interval Word8

maxScalarValues :: [Int]
maxScalarValues = [0x7f, 0x7ff, 0xffff, 0x10ffff]

surrogateRange :: Interval Int
surrogateRange = 0xd800 ... 0xdfff

intersectsWithMaxScalar :: ScalarRange -> Int -> Bool
intersectsWithMaxScalar i max = I.inf i <= max && max < I.sup i

splitMaxScalarValues :: ScalarRange -> [ScalarRange]
splitMaxScalarValues = go maxScalarValues
  where
    go [] r = [r]
    go (max : ms) i =
      -- if it overlaps with max and is not (max ... max), split it
      if intersectsWithMaxScalar i max
        then (I.inf i ... max) : go ms (max + 1 ... I.sup i)
        else go ms i

splitSurrogate :: ScalarRange -> [ScalarRange]
splitSurrogate i
  | Just _ <- i `I.intersection` surrogateRange =
      Foldable.toList (I.interval (I.inf i) 0xd7ff)
        ++ Foldable.toList (I.interval 0xe000 (I.sup i))
  | otherwise = [i]

data Utf8Amount a
  = UOne !a
  | UTwo !a !a
  | UThree !a !a !a
  | UFour !a !a !a !a
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

-- type Utf8Sequence' = Utf8Amount Word8
type Utf8Sequence = Utf8Amount Utf8Range

-- | Returns true if and only if a prefix of bytes matches this sequence
-- of byte ranges.
matchUtf8Sequence :: Utf8Sequence -> ByteString -> Bool
matchUtf8Sequence ranges bs
  | B.length bs < length ranges = False
  | otherwise = all (uncurry I.member) $ zip (B.unpack bs) $ Foldable.toList ranges

utf8AmountFromList :: [a] -> Maybe (Utf8Amount a)
utf8AmountFromList = \case
  [a] -> Just $ UOne a
  [a, b] -> Just $ UTwo a b
  [a, b, c] -> Just $ UThree a b c
  [a, b, c, d] -> Just $ UFour a b c d
  _ -> Nothing

encodedRangeToUtf8Sequence :: ([Word8], [Word8]) -> Maybe Utf8Sequence
encodedRangeToUtf8Sequence = \case
  ([a], [a']) -> Just $ UOne (a ... a')
  ([a, b], [a', b']) -> Just $ UTwo (a ... a') (b ... b')
  ([a, b, c], [a', b', c']) -> Just $ UThree (a ... a') (b ... b') (c ... c')
  ([a, b, c, d], [a', b', c', d']) -> Just $ UFour (a ... a') (b ... b') (c ... c') (d ... d')
  _ -> Nothing

utf8Sequences :: ScalarRange -> [Utf8Sequence]
utf8Sequences =
  fmap (Maybe.fromJust . encodedRangeToUtf8Sequence . encodeRange)
    . utf8ScalarRanges

isAsciiRange :: ScalarRange -> Bool
isAsciiRange = (I.<=! 0x7f)

encodeRange :: ScalarRange -> ([Word8], [Word8])
encodeRange i = (e $ I.inf i, e $ I.sup i)
  where
    e = encodeCharUtf8 . Char.chr

-- this will replace surrogates with the unicode replacement character
encodeCharUtf8 :: Char -> [Word8]
encodeCharUtf8 = B.unpack . T.encodeUtf8 . T.singleton

-- | this function will split the 'ScalarRange' into a valid 'ScalarRange',
-- as defined by 'validScalarRange'. These can then be safely encoded into
-- 'Utf8Sequence's.
utf8ScalarRanges :: ScalarRange -> [ScalarRange]
utf8ScalarRanges =
  splitScalarRanges
    <=< splitMaxScalarValues
    <=< splitSurrogate

splitScalarRanges :: ScalarRange -> [ScalarRange]
splitScalarRanges i
  | I.inf i < 0xe000 && I.sup i > 0xd7ff = error $ show i ++ " intersects with surrogate range"
  | isAsciiRange i = [i]
  | otherwise = fromMaybe [i] $ asum $ trySplit <$> [1 :: Int .. 3]
  where
    trySplit numUtf8Bytes = do
      let m :: Int = (1 `unsafeShiftL` (6 * numUtf8Bytes)) - 1
      let m' = complement m
      if I.inf i .&. m' /= I.sup i .&. m'
        then
          if
              | I.inf i .&. m /= 0 ->
                  Just $
                    splitScalarRanges (I.inf i ... I.inf i .|. m)
                      ++ splitScalarRanges ((I.inf i .|. m) + 1 ... I.sup i)
              | I.sup i .&. m /= m ->
                  Just $
                    splitScalarRanges (I.inf i ... (I.sup i .&. m') - 1)
                      ++ splitScalarRanges (I.sup i .&. m' ... I.sup i)
              | otherwise -> Nothing
        else Just [i]

toScalarRange :: Interval Char -> Interval Int
toScalarRange i = Char.ord (I.inf i) ... Char.ord (I.sup i)

validScalarRange :: ScalarRange -> Bool
validScalarRange i = do
  Maybe.isNothing (i `I.intersection` surrogateRange)
    && not (any (intersectsWithMaxScalar i) maxScalarValues)
    && any valid [1 :: Int .. 3]
  where
    valid numUtf8Bytes = do
      let m :: Int = (1 `unsafeShiftL` (6 * numUtf8Bytes)) - 1
      let m' = complement m
      (I.inf i .&. m' == I.sup i .&. m') || (I.inf i .&. m == 0 || I.sup i .&. m /= m)

-- this is useful because it will not replace surrogates with the unicode replacement character
encodeCharUtf8WithInvalid :: Char -> ByteString
encodeCharUtf8WithInvalid = encodeScalarValueWithInvalid . Char.ord

encodeScalarValueWithInvalid :: Int -> ByteString
encodeScalarValueWithInvalid = B.pack . map fromIntegral . go
  where
    go cp
      | cp <= 0x7f = [cp]
      | cp <= 0x7ff =
          [ 0xc0 + (cp `shiftR` 6),
            0x80 + cp .&. 0x3f
          ]
      | cp <= 0xffff =
          [ 0xe0 + (cp `shiftR` 12),
            0x80 + ((cp `shiftR` 6) .&. 0x3f),
            0x80 + cp .&. 0x3f
          ]
      | otherwise =
          [ 0xf0 + (cp `shiftR` 18),
            0x80 + ((cp `shiftR` 12) .&. 0x3f),
            0x80 + ((cp `shiftR` 6) .&. 0x3f),
            0x80 + cp .&. 0x3f
          ]
