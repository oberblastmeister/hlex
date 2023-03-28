{-# LANGUAGE TemplateHaskell #-}

module Ilex.Internal.Driver where

import Data.Function (on)
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

ilex :: TH.ExpQ -> TH.ExpQ -> Rule.RuleBuilder () -> TH.ExpQ
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
    Table -> Backend.Table.codegen backendConfig minDfa'
    _ -> undefined
    -- Fun -> Backend.Fun.codegen backendConfig minDfa
  where
    backendConfig = Backend.BackendConfig {onError, onEof}
    rules' = (\(i, rule) -> Indexed i <$> rule) <$> zip [0 :: Int ..] rules
    nfa = RegexToNfa.lexerToNfa rules'
    dfa = NfaToDfa.nfaToDfa nfa
    minDfa = Minimize.minimize dfa
    minDfa' = (fmap . fmap) value minDfa

data Indexed a = Indexed {index :: !Int, value :: a}

instance Eq (Indexed a) where
  (==) = (==) `on` index

instance Ord (Indexed a) where
  compare = compare `on` index

data BackendKind = Table | Fun

data Config = Config
  { backendKind :: BackendKind,
    onError :: TH.ExpQ,
    onEof :: TH.ExpQ,
    rules :: [Rule Rule.Accept]
  }
