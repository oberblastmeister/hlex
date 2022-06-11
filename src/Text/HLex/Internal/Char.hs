module Text.HLex.Internal.Char
  ( encodeUtf8,
    rangeCharToUtf8,
  )
where

import Control.Monad (when)
import Data.Primitive qualified as Primitive
import Data.Text qualified as T
import Data.Text.Array qualified as T.Array
import Data.Text.Internal qualified as T.Internal
import Data.Word (Word8)
import GHC.Exts (toList)
import Text.HLex.Internal.Range (Range, pattern RangeV)
import Text.HLex.Internal.Range qualified as Range

-- list is or, inner list of bytes is and
rangeCharToUtf8 :: Range Char -> [Range [Word8]]
rangeCharToUtf8 (fmap encodeUtf8 -> RangeV start end) = fix start end

fix :: [Word8] -> [Word8] -> [Range [Word8]]
fix x y
  | lenX == 0 || lenY == 0 = error "fix: incorrect input given"
  | lenX == lenY = [Range.new x y]
  | lenX == 1 = Range.new x [0x7F] : fix [0xC2, 0x80] y
  | lenX == 2 = Range.new x [0xDF, 0xBF] : fix [0xE0, 0x80, 0x80] y
  | lenX == 3 = Range.new x [0xEF, 0xBF, 0xBF] : fix [0xF0, 0x80, 0x80, 0x80] y
  | otherwise = error "fix: incorrect input given"
  where
    lenX = length x
    lenY = length y

encodeUtf8 :: Char -> [Word8]
encodeUtf8 c = case T.cons c T.empty of
  T.Internal.Text (T.Array.ByteArray bs) off len -> do
    when (off /= 0) $ error "offset should be zero"
    let barr = Primitive.ByteArray bs
    when (len /= Primitive.sizeofByteArray barr) $ error "len should be equal to byte array length"
    toList barr
