{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Hlex.Internal.Rule where

import Control.Monad.State.Strict (MonadState, State)
import Control.Monad.State.Strict qualified as State
import Hlex.Internal.Regex
import Language.Haskell.TH qualified as TH

type Accept = Accept' TH.ExpQ

data Accept' a = Accept
  { exp :: a,
    context :: Maybe a
  } deriving (Show, Eq)

defRules :: Rules' a
defRules =
  Rules
    { rulesList = [],
      rulesAny = Nothing,
      rulesEof = Nothing,
      rulesInvalidUtf8 = Nothing
    }

type Rules = Rules' TH.ExpQ

data Rules' a = Rules
  { rulesList :: [Rule (Accept' a)],
    rulesAny :: Maybe a,
    rulesEof :: Maybe a,
    rulesInvalidUtf8 :: Maybe a
  } deriving (Show, Eq)

type RuleBuilder = RuleBuilder' TH.ExpQ

newtype RuleBuilder' a b = RuleBuilder
  { runLexerBuilder :: State (Rules' a) b
  }
  deriving
    (Functor, Applicative, Monad, MonadState (Rules' a))
    via (State (Rules' a))

data Rule a = Rule
  { regex :: Regex,
    accept :: a
  }
  deriving (Show, Eq, Functor)

modifyRules :: MonadState (Rules' a) m => ([Rule (Accept' a)] -> [Rule (Accept' a)]) -> m ()
modifyRules f = State.modify' \rules -> rules {rulesList = f $ rulesList rules}

rule :: Regex -> a -> RuleBuilder' a ()
rule regex exp = modifyRules (Rule {regex, accept = Accept {exp, context = Nothing}} :)

ruleContext :: Regex -> (a, a) -> RuleBuilder' a ()
ruleContext regex (exp, context) = modifyRules (Rule {regex, accept = Accept {exp, context = Just context}} :)

ruleAny :: a -> RuleBuilder' a ()
ruleAny exp = State.modify' \rules -> rules {rulesAny = Just exp}

ruleEof :: a -> RuleBuilder' a ()
ruleEof exp = State.modify' \rules -> rules {rulesEof = Just exp}

ruleCatchAll :: a -> RuleBuilder' a ()
ruleCatchAll exp = ruleAny exp *> ruleEof exp

ruleInvalidUtf8 :: a -> RuleBuilder' a ()
ruleInvalidUtf8 exp = State.modify' \rules -> rules {rulesInvalidUtf8 = Just exp}

(~=) :: Regex -> a -> RuleBuilder' a ()
(~=) = rule

(~=?) :: Regex -> (a, a) -> RuleBuilder' a ()
(~=?) = ruleContext

data SpecialRule = CatchAll | OnAny | OnEof | OnInvalidUtf8

ruleSpecial :: SpecialRule -> a -> RuleBuilder' a ()
ruleSpecial CatchAll = ruleCatchAll
ruleSpecial OnAny = ruleAny
ruleSpecial OnEof = ruleEof
ruleSpecial OnInvalidUtf8 = ruleInvalidUtf8

(~=!) :: SpecialRule -> a -> RuleBuilder' a ()
(~=!) = ruleSpecial

evalRuleBuilder :: RuleBuilder' a () -> Rules' a
evalRuleBuilder m = rules {rulesList = reverse $ rulesList rules}
  where
    rules = flip State.execState defRules . runLexerBuilder $ m
