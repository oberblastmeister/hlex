module Ilex.Internal.Regex.Parser where

import Data.Text (Text)
import Data.Void (Void)
import Ilex.Internal.Regex (Regex)
import Ilex.Internal.Regex qualified as RE
import Text.Megaparsec (choice)
import Text.Megaparsec qualified as P

type Parser = P.Parsec Void Text

regex :: Parser Regex
regex = undefined

atom :: Parser Regex
atom = choice []
