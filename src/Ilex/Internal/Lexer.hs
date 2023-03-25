module Ilex.Internal.Lexer where

import Ilex.Internal.Regex

newtype Lexer a = Lexer {rules :: [Rule a]}

-- newtype LexerBuilder a b = LexerBuilder
--   { runLexerBuilder :: Writer (Dual [RuleBuild a]) b
--   }
--   deriving
--     (Functor, Applicative, Monad, MonadWriter (Dual [RuleBuild a]))
--     via (Writer (Dual [RuleBuild a]))

-- data RuleBuild a = RuleBuild
--   { regex :: Core.Regex,
--     ruleCode :: Code Q a
--   }

data Rule a = Rule
  { regex :: Regex,
    accept :: a
  }

data Accept a = Accept
  { value :: a,
    priority :: !Int
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- (~=) :: Dsl.Regex () -> Code Q a -> LexerBuilder a ()
-- re ~= code = tell $ Dual [RuleBuild {regex = Dsl.runRegex re, ruleCode = code}]

-- evalLexerBuilder :: LexerBuilder a () -> Lexer a
-- evalLexerBuilder =
--   Lexer
--     . fmap (\(i, RuleBuild {regex, ruleCode}) -> Rule {regex, accept = Accept {value = ruleCode, priority = i}})
--     . zip [1 ..]
--     . reverse
--     . getDual
--     . execWriter
--     . runLexerBuilder
