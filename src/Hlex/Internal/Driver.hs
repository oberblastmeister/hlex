{-# LANGUAGE TemplateHaskell #-}

module Hlex.Internal.Driver
  ( hlex,
    hlexFromConfig,
    Config (..),
    BackendKind,
    matches,
  )
where

import Data.Function (on)
import Hlex.Internal.Backend qualified as Backend
import Hlex.Internal.Backend.Table qualified as Backend.Table
import Hlex.Internal.Minimize qualified as Minimize
import Hlex.Internal.NfaToDfa qualified as NfaToDfa
import Hlex.Internal.Regex (Regex)
import Hlex.Internal.Regex qualified as Regex
import Hlex.Internal.RegexToNfa qualified as RegexToNfa
import Hlex.Internal.Rule (Rules)
import Hlex.Internal.Rule qualified as Rule
import Language.Haskell.TH qualified as TH
import Prelude hiding (lex)

hlex :: Rule.RuleBuilder () -> TH.ExpQ
hlex builder =
  hlexFromConfig
    Config
      { backendKind = Table,
        rules = Rule.evalRuleBuilder builder
      }

matches :: Regex -> TH.ExpQ
matches r = Backend.Table.matchesDfa minDfa
  where
    nfa = RegexToNfa.regexToNfa () r
    dfa = NfaToDfa.nfaToDfa nfa
    minDfa = Minimize.minimize dfa

hlexFromConfig :: Config -> TH.ExpQ
hlexFromConfig Config {backendKind, rules} = do
  anyRule <- case Rule.rulesAny rules of
    Nothing -> fail "lexer does not cover all cases"
    Just exp -> pure exp
  eofRule <- case Rule.rulesEof rules of
    Nothing -> fail "lexer does not cover eof case"
    Just exp -> pure exp
  let backendConfig =
        Backend.BackendConfig
          { onInvalidUtf8 = Rule.rulesInvalidUtf8 rules,
            onEof = eofRule
          }
      -- this is necessary because if no bytes match, the lexer will just return and skip the current byte
      -- this is bad if the input is utf8 because it could skip the the middle of a multi-byte character
      -- this can cause the invalid utf-8 to be given to the error handler
      -- instead, we add a catch all case using Regex.dot'.
      -- This will always match because 'Text' is always guaranteed to be valid utf8
      -- In the case of 'ByteString'
      rulesWithError =
        Rule.rulesList rules
          ++ [ Rule.Rule
                 { regex = Regex.dot',
                   accept = Rule.Accept {exp = anyRule, context = Nothing}
                 }
             ]
      rulesList' = (\(i, rule) -> Indexed i <$> rule) <$> zip [0 :: Int ..] rulesWithError
      nfa = RegexToNfa.lexerToNfa rulesList'
      dfa = NfaToDfa.nfaToDfa nfa
      minDfa = Minimize.minimize dfa
      minDfa' = (fmap . fmap) value minDfa
  case backendKind of
    Table -> Backend.Table.codegen backendConfig minDfa'
  where

data Indexed a = Indexed {index :: !Int, value :: a}

instance Eq (Indexed a) where
  (==) = (==) `on` index

instance Ord (Indexed a) where
  compare = compare `on` index

data BackendKind = Table

data Config = Config
  { backendKind :: BackendKind,
    rules :: Rules
  }
