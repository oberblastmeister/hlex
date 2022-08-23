module Text.HLex.Internal.Lexer where

import Control.Monad.Writer
import Language.Haskell.TH (Code, Q)
import Text.HLex.Internal.Accept (Accept (..))
import Text.HLex.Internal.Regex.Core qualified as Core

newtype Lexer a = Lexer {rules :: [Rule a]}

newtype LexerBuilder a b = LexerBuilder
  { runLexerBuilder :: Writer (Dual [RuleBuild a]) b
  }
  deriving
    (Functor, Applicative, Monad, MonadWriter (Dual [RuleBuild a]))
    via (Writer (Dual [RuleBuild a]))

data RuleBuild a = RuleBuild
  { regex :: Core.Regex,
    ruleCode :: Code Q a
  }

data Rule a = Rule
  { regex :: Core.Regex,
    accept :: Accept (Code Q a)
  }

-- (~=) :: Dsl.Regex () -> Code Q a -> LexerBuilder a ()
-- re ~= code = tell $ Dual [RuleBuild {regex = Dsl.runRegex re, ruleCode = code}]

evalLexerBuilder :: LexerBuilder a () -> Lexer a
evalLexerBuilder =
  Lexer
    . fmap (\(i, RuleBuild {regex, ruleCode}) -> Rule {regex, accept = Accept {value = ruleCode, priority = i}})
    . zip [1 ..]
    . reverse
    . getDual
    . execWriter
    . runLexerBuilder
