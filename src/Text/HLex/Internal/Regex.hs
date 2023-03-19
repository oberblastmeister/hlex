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

isEmpty :: Regex -> Bool
isEmpty Empty = True
isEmpty (Set _) = False
isEmpty (Rep r) = isEmpty r
isEmpty (Cat r1 r2) = isEmpty r1 && isEmpty r2
isEmpty (Alt r1 r2) = isEmpty r1 && isEmpty r2

empty :: Regex
empty = Empty

set :: CharSet -> Regex
set = Set

when :: (Char -> Bool) -> Regex
when = Set . CharSet.fromPred

string :: String -> Regex
string = foldMap (Set . CharSet.singleton)

optional :: Regex -> Regex
optional = Alt empty

many :: Regex -> Regex
many = Rep

some :: Regex -> Regex
some r = r <> many r

exactly :: Int -> Regex -> Regex
exactly n r = fold $ replicate n r

atLeast :: Int -> Regex -> Regex
atLeast n r = exactly n r <> many r
