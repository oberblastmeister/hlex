module Ilex.Internal.Driver where

import Data.Vector qualified as VB
import Ilex.Internal.Rule qualified as Rule
import Ilex.Internal.Minimize qualified as Minimize
import Ilex.Internal.NfaToDfa qualified as NfaToDfa
import Ilex.Internal.RegexToNfa qualified as RegexToNfa
import Language.Haskell.TH qualified as TH
import Prelude hiding (lex)
import qualified Ilex.Internal.Backend as Backend
import qualified Ilex.Internal.Backend.Table as Backend.Table
import qualified Ilex.Internal.Backend.Fun as Backend.Fun

ilex :: TH.ExpQ -> TH.ExpQ -> Rule.RuleBuilder TH.ExpQ () -> TH.ExpQ
ilex onError onEof builder =
  Backend.Table.codegen
    Backend.BackendConfig
      { acceptMap,
        onError,
        onEof
      }
    minDfa
  where
    lexer = Rule.evalRuleBuilder builder
    lexer' = (\(i, rule) -> rule {Rule.accept = i}) <$> zip [0 :: Int ..] lexer
    acceptMap = VB.fromList $ Rule.accept <$> lexer
    nfa = RegexToNfa.lexerToNfa lexer'
    dfa = NfaToDfa.nfaToDfa nfa
    minDfa = Minimize.minimize dfa
