module Text.HLex.Internal.Lexer where

import Control.Monad.Writer
import Language.Haskell.TH (Code, Q)
import Text.HLex.Internal.Regex.Core qualified as Core
import Text.HLex.Internal.Regex.Dsl qualified as Dsl

newtype Lexer a = Lexer {rules :: [Rule a]}

newtype LexerBuilder a b = LexerBuilder {runLexerBuilder :: Writer (Dual [Rule a]) b}
  deriving (Functor, Applicative, Monad, MonadWriter (Dual [Rule a])) via (Writer (Dual [Rule a]))

data Rule a = Rule {regex :: Core.Regex, code :: Code Q a}

(~=) :: Dsl.Regex () -> Code Q a -> LexerBuilder a ()
re ~= code = tell $ Dual [Rule {regex = Dsl.runRegex re, code}]

evalLexerBuilder :: LexerBuilder a () -> Lexer a
evalLexerBuilder = Lexer . reverse . getDual . execWriter . runLexerBuilder
