{-# LANGUAGE TemplateHaskell #-}

module Ilex.Internal.Driver
  ( ilex,
    ilexFromConfig,
    Config (..),
    BackendKind,
    matches,
  )
where

import Data.Function (on)
import Ilex.Internal.Backend qualified as Backend
import Ilex.Internal.Backend.Table qualified as Backend.Table
import Ilex.Internal.Minimize qualified as Minimize
import Ilex.Internal.NfaToDfa qualified as NfaToDfa
import Ilex.Internal.Regex (Regex)
import Ilex.Internal.Regex qualified as Regex
import Ilex.Internal.RegexToNfa qualified as RegexToNfa
import Ilex.Internal.Rule (Rules)
import Ilex.Internal.Rule qualified as Rule
import Language.Haskell.TH qualified as TH
import Prelude hiding (lex)

ilex :: Rule.RuleBuilder () -> TH.ExpQ
ilex builder =
  ilexFromConfig
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

ilexFromConfig :: Config -> TH.ExpQ
ilexFromConfig Config {backendKind, rules} = do
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
