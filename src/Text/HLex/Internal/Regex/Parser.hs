module Text.HLex.Internal.Regex.Parser where

import Data.Text (Text)
import Data.Void (Void)
import Text.HLex.Internal.Regex (Regex)
import Text.HLex.Internal.Regex qualified as RE
import Text.Megaparsec (choice)
import Text.Megaparsec qualified as P

type Parser = P.Parsec Void Text

regex :: Parser Regex
regex = undefined

atom :: Parser Regex
atom = choice []
