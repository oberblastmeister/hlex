{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Ilex.Internal.Rule where

import Control.Monad.State.Strict (MonadState, State)
import Control.Monad.State.Strict qualified as State
import Ilex.Internal.Regex
import Language.Haskell.TH qualified as TH

data Accept = Accept
  { exp :: TH.ExpQ,
    context :: Maybe TH.ExpQ
  }

type RuleBuilder = RuleBuilder' Accept

newtype RuleBuilder' a b = RuleBuilder
  { runLexerBuilder :: State ([Rule a]) b
  }
  deriving
    (Functor, Applicative, Monad, MonadState [Rule a])
    via (State [Rule a])

data Rule a = Rule
  { regex :: Regex,
    accept :: a
  }
  deriving (Functor)

rule :: Regex -> TH.ExpQ -> RuleBuilder ()
rule regex exp = State.modify (Rule {regex, accept = Accept {exp, context = Nothing}} :)

ruleContext :: Regex -> (TH.ExpQ, TH.ExpQ) -> RuleBuilder ()
ruleContext regex (exp, context) = State.modify (Rule {regex, accept = Accept {exp, context = Just context}} :)

(~=) :: Regex -> TH.ExpQ -> RuleBuilder ()
(~=) = rule

(~=?) :: Regex -> (TH.ExpQ, TH.ExpQ) -> RuleBuilder ()
(~=?) = ruleContext

evalRuleBuilder :: RuleBuilder () -> [Rule Accept]
evalRuleBuilder = reverse . flip State.execState [] . runLexerBuilder
