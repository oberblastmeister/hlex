{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Ilex.Internal.Lexer where

import Control.Monad.Writer.CPS (MonadWriter, Writer)
import Control.Monad.Writer.CPS qualified as Writer
import Data.Monoid (Dual (..))
import Ilex.Internal.Monad (LexerInput#)
import Ilex.Internal.Regex
import Language.Haskell.TH qualified as TH

newtype LexerBuilder a b = LexerBuilder
  { runLexerBuilder :: Writer (Dual [Rule a]) b
  }
  deriving
    (Functor, Applicative, Monad, MonadWriter (Dual [Rule a]))
    via (Writer (Dual [Rule a]))

data Rule a = Rule
  { regex :: Regex,
    accept :: a
  }

ruleWith :: Regex -> TH.ExpQ -> LexerBuilder TH.ExpQ ()
ruleWith regex accept = Writer.tell $ Dual [Rule {regex, accept}]

rule :: Regex -> TH.ExpQ -> LexerBuilder TH.ExpQ ()
rule regex accept = ruleWith regex [|\(_ :: LexerInput#) -> $accept|]

(~>) :: Regex -> TH.ExpQ -> LexerBuilder TH.ExpQ ()
(~>) = ruleWith

(~=) :: Regex -> TH.ExpQ -> LexerBuilder TH.ExpQ ()
(~=) = rule

evalLexerBuilder :: LexerBuilder a () -> [Rule a]
evalLexerBuilder = reverse . getDual . Writer.execWriter . runLexerBuilder
