{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Hlex.Internal.Rule where

import Control.Monad.State.Strict (MonadState, State)
import Control.Monad.State.Strict qualified as State
import Hlex.Internal.Regex
import Language.Haskell.TH qualified as TH

data Accept = Accept
  { exp :: TH.ExpQ,
    context :: Maybe TH.ExpQ
  }

defRules :: Rules
defRules =
  Rules
    { rulesList = [],
      rulesAny = Nothing,
      rulesEof = Nothing,
      rulesInvalidUtf8 = Nothing
    }

data Rules = Rules
  { rulesList :: [Rule Accept],
    rulesAny :: Maybe TH.ExpQ,
    rulesEof :: Maybe TH.ExpQ,
    rulesInvalidUtf8 :: Maybe TH.ExpQ
  }

newtype RuleBuilder b = RuleBuilder
  { runLexerBuilder :: State Rules b
  }
  deriving
    (Functor, Applicative, Monad, MonadState Rules)
    via (State Rules)

data Rule a = Rule
  { regex :: Regex,
    accept :: a
  }
  deriving (Functor)

modifyRules :: MonadState Rules m => ([Rule Accept] -> [Rule Accept]) -> m ()
modifyRules f = State.modify' \rules -> rules {rulesList = f $ rulesList rules}

rule :: Regex -> TH.ExpQ -> RuleBuilder ()
rule regex exp = modifyRules (Rule {regex, accept = Accept {exp, context = Nothing}} :)

ruleContext :: Regex -> (TH.ExpQ, TH.ExpQ) -> RuleBuilder ()
ruleContext regex (exp, context) = modifyRules (Rule {regex, accept = Accept {exp, context = Just context}} :)

ruleAny :: TH.ExpQ -> RuleBuilder ()
ruleAny exp = State.modify' \rules -> rules {rulesAny = Just exp}

ruleEof :: TH.ExpQ -> RuleBuilder ()
ruleEof exp = State.modify' \rules -> rules {rulesEof = Just exp}

ruleCatchAll :: TH.ExpQ -> RuleBuilder ()
ruleCatchAll exp = ruleAny exp *> ruleEof exp

ruleInvalidUtf8 :: TH.ExpQ -> RuleBuilder ()
ruleInvalidUtf8 exp = State.modify' \rules -> rules {rulesInvalidUtf8 = Just exp}

(~=) :: Regex -> TH.ExpQ -> RuleBuilder ()
(~=) = rule

(~=?) :: Regex -> (TH.ExpQ, TH.ExpQ) -> RuleBuilder ()
(~=?) = ruleContext

data SpecialRule = CatchAll | OnAny | OnEof | OnInvalidUtf8

ruleSpecial :: SpecialRule -> TH.ExpQ -> RuleBuilder ()
ruleSpecial CatchAll = ruleCatchAll
ruleSpecial OnAny = ruleAny
ruleSpecial OnEof = ruleEof
ruleSpecial OnInvalidUtf8 = ruleInvalidUtf8

(~=!) :: SpecialRule -> TH.ExpQ -> RuleBuilder ()
(~=!) = ruleSpecial

evalRuleBuilder :: RuleBuilder () -> Rules
evalRuleBuilder m = rules {rulesList = reverse $ rulesList rules}
  where
    rules = flip State.execState defRules . runLexerBuilder $ m
