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

matches :: Regex -> TH.ExpQ
matches r = Backend.Table.matchesDfa minDfa
  where
    nfa = RegexToNfa.regexToNfa () r
    dfa = NfaToDfa.nfaToDfa nfa
    minDfa = Minimize.minimize dfa

ilexFromConfig :: Config -> TH.ExpQ
ilexFromConfig Config {backendKind, onError, onEof, rules} =
  case backendKind of
    Table -> Backend.Table.codegen backendConfig minDfa'
  where
    backendConfig = Backend.BackendConfig {onInvalidUtf8 = onError, onEof}
    -- this is necessary because if no bytes match, the lexer will just return and skip the current byte
    -- this is bad if the input is utf8 because it could skip the the middle of a multi-byte character
    -- this can cause the invalid utf-8 to be given to the error handler
    -- instead, we add a catch all case using Regex.dot'.
    -- This will always match because 'Text' is always guaranteed to be valid utf8
    -- In the case of 'ByteString'
    rulesWithError =
      rules
        ++ [ Rule.Rule
               { regex = Regex.dot',
                 accept = Rule.Accept {exp = onError, context = Nothing}
               }
           ]
    rules' = (\(i, rule) -> Indexed i <$> rule) <$> zip [0 :: Int ..] rulesWithError
    nfa = RegexToNfa.lexerToNfa rules'
    dfa = NfaToDfa.nfaToDfa nfa
    minDfa = Minimize.minimize dfa
    minDfa' = (fmap . fmap) value minDfa

data Indexed a = Indexed {index :: !Int, value :: a}

instance Eq (Indexed a) where
  (==) = (==) `on` index

instance Ord (Indexed a) where
  compare = compare `on` index

data BackendKind = Table

data Config = Config
  { backendKind :: BackendKind,
    onError :: TH.ExpQ,
    onEof :: TH.ExpQ,
    rules :: [Rule Rule.Accept]
  }
