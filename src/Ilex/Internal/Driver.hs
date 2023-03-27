{-# LANGUAGE TemplateHaskell #-}

module Ilex.Internal.Driver where

import Data.Vector qualified as VB
import Ilex.Internal.Backend qualified as Backend
import Ilex.Internal.Backend.Fun qualified as Backend.Fun
import Ilex.Internal.Backend.Table qualified as Backend.Table
import Ilex.Internal.Minimize qualified as Minimize
import Ilex.Internal.NfaToDfa qualified as NfaToDfa
import Ilex.Internal.RegexToNfa qualified as RegexToNfa
import Ilex.Internal.Rule (Rule)
import Ilex.Internal.Rule qualified as Rule
import Language.Haskell.TH qualified as TH
import Prelude hiding (lex)

ilex :: TH.ExpQ -> TH.ExpQ -> Rule.RuleBuilder TH.ExpQ () -> TH.ExpQ
ilex onError onEof builder =
  ilexFromConfig
    Config
      { backendKind = Table,
        onError,
        onEof,
        rules = Rule.evalRuleBuilder builder
      }

ilexFromConfig :: Config -> TH.ExpQ
ilexFromConfig Config {backendKind, onError, onEof, rules} =
  case backendKind of
    Fun -> Backend.Fun.codegen backendConfig minDfa
    Table -> Backend.Table.codegen backendConfig minDfa
  where
    backendConfig = Backend.BackendConfig {acceptMap, onError, onEof}
    rules' = (\(i, rule) -> rule {Rule.accept = i}) <$> zip [0 :: Int ..] rules
    acceptMap = VB.fromList $ Rule.accept <$> rules
    nfa = RegexToNfa.lexerToNfa rules'
    dfa = NfaToDfa.nfaToDfa nfa
    minDfa = Minimize.minimize dfa

data BackendKind = Table | Fun

data Config = Config
  { backendKind :: BackendKind,
    onError :: TH.ExpQ,
    onEof :: TH.ExpQ,
    rules :: [Rule TH.ExpQ]
  }
