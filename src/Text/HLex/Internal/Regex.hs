module Text.HLex.Internal.Regex where

import Text.HLex.Internal.CharSet (CharSet)
import Text.HLex.Internal.CharSet qualified as CharSet

data Regex
  = Empty
  | Set CharSet
  | Star Regex
  | Cat Regex Regex
  | Alt Regex Regex
  deriving (Show, Eq)

instance Semigroup Regex where
  (<>) = Cat

instance Monoid Regex where
  mempty = empty

empty :: Regex
empty = Set CharSet.empty

string :: String -> Regex
string = foldr (Cat . Set . CharSet.singleton) Empty
