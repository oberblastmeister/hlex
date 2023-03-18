module Text.HLex.Internal.Regex where

import Data.Foldable (fold)
import Text.HLex.Internal.CharSet (CharSet)
import Text.HLex.Internal.CharSet qualified as CharSet

data Regex
  = Empty
  | Set CharSet
  | Rep Regex
  | Cat Regex Regex
  | Alt Regex Regex
  deriving (Show, Eq)

instance Semigroup Regex where
  (<>) = Cat

instance Monoid Regex where
  mempty = empty

empty :: Regex
empty = Empty

string :: String -> Regex
string = foldMap (Set . CharSet.singleton)

optional :: Regex -> Regex
optional = Alt Empty

many :: Regex -> Regex
many = Rep

some :: Regex -> Regex
some r = r <> many r

exactly :: Int -> Regex -> Regex
exactly n r = fold $ replicate n r

atLeast :: Int -> Regex -> Regex
atLeast n r = exactly n r <> many r
