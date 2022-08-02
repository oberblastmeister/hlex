module Text.HLex.Internal.Regex.Syntax where

import Data.Text (Text)

data Regex a
  = Empty
  | Lit !Text
  | Range !Char !Char
  | Rep (Regex a)
  | Concat (Regex a) (Regex a)
  | Alt (Regex a) (Regex a)
  deriving (Show, Eq)
