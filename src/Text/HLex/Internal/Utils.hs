module Text.HLex.Internal.Utils where

import Data.Bifunctor (Bifunctor, bimap)
import Data.RangeSet.List (RSet)
import Data.Word (Word8)

type Byte = Word8

type ByteSet = RSet Byte

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f
