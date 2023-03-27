{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Ilex.Internal.Rule where

import Control.Monad.Writer.CPS (MonadWriter, Writer)
import Control.Monad.Writer.CPS qualified as Writer
import Data.Monoid (Dual (..))
import Ilex.Internal.Regex
import Language.Haskell.TH qualified as TH

newtype RuleBuilder a b = RuleBuilder
  { runLexerBuilder :: Writer (Dual [Rule a]) b
  }
  deriving
    (Functor, Applicative, Monad, MonadWriter (Dual [Rule a]))
    via (Writer (Dual [Rule a]))

data Rule a = Rule
  { regex :: Regex,
    accept :: a
  }

rule :: Regex -> TH.ExpQ -> RuleBuilder TH.ExpQ ()
rule regex accept = Writer.tell $ Dual [Rule {regex, accept}]

(~=) :: Regex -> TH.ExpQ -> RuleBuilder TH.ExpQ ()
(~=) = rule

evalRuleBuilder :: RuleBuilder a () -> [Rule a]
evalRuleBuilder = reverse . getDual . Writer.execWriter . runLexerBuilder
