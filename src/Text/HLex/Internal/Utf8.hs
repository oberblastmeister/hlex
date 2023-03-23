{-# LANGUAGE MultiWayIf #-}

module Text.HLex.Internal.Utf8 where

import Control.Applicative (asum)
import Control.Monad ((<=<))
import Data.Bits (complement, unsafeShiftL, (.&.), (.|.))
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

maxUtf8Bytes :: Int
maxUtf8Bytes = 4

maxScalarValues :: [Int]
maxScalarValues = [0x7f, 0x7ff, 0xffff, 0x10ffff]

surrogateRange :: Interval Int
surrogateRange = 0xd800 ... 0xdfff

splitMaxScalarValues :: ScalarRange -> [ScalarRange]
splitMaxScalarValues = go maxScalarValues
  where
    go [] r = [r]
    go (max : ms) i =
      -- if it overlaps with max and is not (max ... max), split it
      if I.inf i <= max && max < I.sup i
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
matchUtf8Sequence :: Utf8Sequence -> [Word8] -> Bool
matchUtf8Sequence ranges bs
  | length bs < length ranges = False
  | otherwise = all (uncurry I.member) $ zip bs $ Foldable.toList ranges

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
    . utf8Sequences'
    <=< splitMaxScalarValues
    <=< splitSurrogate

isAsciiRange :: ScalarRange -> Bool
isAsciiRange = (I.<=! 0x7f)

encodeRange :: ScalarRange -> ([Word8], [Word8])
encodeRange i = (e $ I.inf i, e $ I.sup i)
  where
    e = encodeCharUtf8 . Char.chr

encodeCharUtf8 :: Char -> [Word8]
encodeCharUtf8 = B.unpack . T.encodeUtf8 . T.singleton

utf8Sequences' :: ScalarRange -> [ScalarRange]
utf8Sequences' i
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
                    utf8Sequences' (I.inf i ... I.inf i .|. m)
                      ++ utf8Sequences' ((I.inf i .|. m) + 1 ... I.sup i)
              | I.sup i .&. m /= m ->
                  Just $
                    utf8Sequences' (I.inf i ... (I.sup i .&. m') - 1)
                      ++ utf8Sequences' (I.sup i .&. m' ... I.sup i)
              | otherwise -> Nothing
        else Just [i]

toScalarRange :: Interval Char -> Interval Int
toScalarRange i = Char.ord (I.inf i) ... Char.ord (I.sup i)
