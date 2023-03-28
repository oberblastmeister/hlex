{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Ilex.Internal.Rule where

import Control.Monad.Writer.CPS (MonadWriter, Writer)
import Control.Monad.Writer.CPS qualified as Writer
import Data.Monoid (Dual (..))
import Ilex.Internal.Regex
import Language.Haskell.TH qualified as TH

data Accept = Accept
  { exp :: TH.ExpQ,
    context :: [TH.ExpQ]
  }

type RuleBuilder = RuleBuilder' Accept

newtype RuleBuilder' a b = RuleBuilder
  { runLexerBuilder :: Writer (Dual [Rule a]) b
  }
  deriving
    (Functor, Applicative, Monad, MonadWriter (Dual [Rule a]))
    via (Writer (Dual [Rule a]))

data Rule a = Rule
  { regex :: Regex,
    accept :: a
  } deriving (Functor)

rule :: Regex -> TH.ExpQ -> RuleBuilder ()
rule regex exp = Writer.tell $ Dual [Rule {regex, accept = Accept {exp, context = []}}]

data Context
  = ContextRegex Regex
  | ContextPred TH.ExpQ

ruleContext :: Regex -> (TH.ExpQ, [TH.ExpQ]) -> RuleBuilder ()
ruleContext regex (exp, context) = Writer.tell $ Dual [Rule {regex, accept = Accept {exp, context}}]

(~=) :: Regex -> TH.ExpQ -> RuleBuilder ()
(~=) = rule

evalRuleBuilder :: RuleBuilder () -> [Rule Accept]
evalRuleBuilder = reverse . getDual . Writer.execWriter . runLexerBuilder
