module Ilex.Internal.Regex
  ( Regex (Null, Empty, ..),
    null,
    isNull,
    alt,
    cat,
    isEmpty,
    empty,
    set,
    range,
    when',
    when,
    string,
    optional,
    many,
    some,
    exactly,
    atLeast,
    dot',
    dot,
    alpha,
    digit,
    isSpace,
    isAsciiSpace,
  )
where

import Data.Char qualified as Char
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.String (IsString (..))
import Ilex.Internal.CharSet (CharSet)
import Ilex.Internal.CharSet qualified as CharSet
import Prelude hiding (null)

data Regex
  = Set CharSet
  | Rep Regex
  | Cat [Regex]
  | Alt (NonEmpty Regex)
  deriving (Show, Eq, Ord)

instance Semigroup Regex where
  r <> r' = cat [r, r']

instance Monoid Regex where
  mempty = empty

instance IsString Regex where
  fromString = string

pattern Null :: Regex
pattern Null <- (isNull -> True)
  where
    Null = Set CharSet.empty

null :: Regex
null = Null

isNull :: Regex -> Bool
isNull (Set (CharSet.null -> True)) = True
isNull _ = False

alt :: [Regex] -> Regex
alt rs = case rs' of
  [r] -> r
  rs -> maybe null Alt $ NE.nonEmpty rs
  where
    rs' = concatMap f rs
    f (Alt rs) = NE.toList rs
    f Null = []
    f r = [r]

cat :: [Regex] -> Regex
cat rs
  | null `elem` rs = null
  | otherwise = case rs' of
      [r] -> r
      rs -> Cat rs
  where
    rs' = concatMap f rs
    f (Cat rs) = rs
    f r = [r]

isEmpty :: Regex -> Bool
isEmpty (Cat []) = True
isEmpty (Cat rs) = any isEmpty rs
isEmpty (Set _) = False
isEmpty (Rep r) = isEmpty r
isEmpty (Alt rs) = any isEmpty rs

pattern Empty :: Regex
pattern Empty <- (isEmpty -> True)
  where
    Empty = Cat []

empty :: Regex
empty = Empty

set :: CharSet -> Regex
set = Set

range :: (Char, Char) -> Regex
range = Set . CharSet.singletonRange

when' :: (Char -> Bool) -> Regex
when' = Set . CharSet.fromPred

when :: (Char -> Bool) -> Regex
when f = Set $ CharSet.fromPred f CharSet.\\ CharSet.singleton '\n'

string :: String -> Regex
string = foldMap (Set . CharSet.singleton)

optional :: Regex -> Regex
optional r = alt [r, empty]

many :: Regex -> Regex
many = Rep

some :: Regex -> Regex
some r = r <> many r

exactly :: Int -> Regex -> Regex
exactly n r = fold $ replicate n r

atLeast :: Int -> Regex -> Regex
atLeast n r = exactly n r <> many r

dot' :: Regex
dot' = Set $ CharSet.singletonRange (Char.chr 0x0, Char.chr 0x10ffff)

dot :: Regex
dot =
  Set $
    CharSet.singletonRange (Char.chr 0x0, Char.chr 0x10ffff)
      CharSet.\\ CharSet.singleton '\n'

alpha :: Regex
alpha = set $ CharSet.singletonRange ('a', 'z') <> CharSet.singletonRange ('A', 'Z')

digit :: Regex
digit = range ('0', '9')

isSpace :: Regex
isSpace = when Char.isSpace

isAsciiSpace :: Regex
isAsciiSpace = when \c -> Char.isAscii c && Char.isSpace c
