module Text.HLex.Internal.Regex.Core where

import Data.Function ((&))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Word (Word8)
import Text.HLex.Internal.Char qualified as Char
import Text.HLex.Internal.Range (Range)
import Text.HLex.Internal.Range qualified as Range

data Regex
  = Empty
  | Range !(Range Word8)
  | Rep Regex
  | Concat Regex Regex
  | Alt Regex Regex
  deriving (Show, Eq)

charRanges :: [Range Char] -> Regex
charRanges rs = foldRE Concat $ charRange <$> rs

charRange :: Range Char -> Regex
charRange range =
  Char.rangeCharToUtf8 range
    & fmap (foldRE Concat . fmap Range . Range.zip)
    & foldRE Alt

zeroOrOne :: Regex -> Regex
zeroOrOne = Alt Empty

zeroOrMore :: Regex -> Regex
zeroOrMore = Rep

oneOrMore :: Regex -> Regex
oneOrMore r = Concat r $ Rep r

exactly :: Int -> Regex -> Regex
exactly n r
  | n < 0 = error "n cannot be negative"
  | otherwise = go n
  where
    go 0 = r
    go n = Concat r $ go $ n - 1

atLeast :: Int -> Regex -> Regex
atLeast n r = Concat (exactly n r) (Rep r)

-- bounded :: Int -> Int -> Regex -> Regex
-- bounded i j
--   | i < 0 = error "i cannot be negative"
--   | j < 0 = error "j cannot be negative"
--   | i > j = error "i must be less than j"
--   | otherwise = [i .. j]

-- replicate :: Regex -> Regex
-- replicate =

-- -- data Repetition = Repetition
-- --   { kind :: !RepetitionKind,
-- --     re :: Regex
-- --   }
-- --   deriving (Show, Eq)

-- data RepetitionKind
--   = ZeroOrOne
--   | ZeroOrMore
--   | OneOrMore
--   | Amount !Amount
--   deriving (Show, Eq)

-- data Amount
--   = Exactly !Int
--   | AtLeast !Int
--   | Bounded !Int !Int
--   deriving (Show, Eq)

text :: Text -> Regex
text =
  foldRE Concat
    . fmap (charRange . Range.point)
    . T.unpack

foldRE :: Foldable t => (Regex -> Regex -> Regex) -> t Regex -> Regex
foldRE f = foldr f Empty
