module Ilex (lex, Lexer.rule, (Lexer.~=), (Lexer.~>), inputText) where

import Data.Vector qualified as VB
import Ilex.Internal.Lexer qualified as Lexer
import Ilex.Internal.Minimize qualified as Minimize
import Ilex.Internal.NfaToDfa qualified as NfaToDfa
import Ilex.Internal.RegexToNfa qualified as RegexToNfa
import Language.Haskell.TH qualified as TH
import Prelude hiding (lex)
import Ilex.Internal.Monad (inputText)
import qualified Ilex.Internal.Backend as Backend
import qualified Ilex.Internal.Backend.Table as Backend.Table

lex :: TH.ExpQ -> TH.ExpQ -> Lexer.LexerBuilder TH.ExpQ () -> TH.ExpQ
lex onError onEof builder =
  Backend.Table.codegen
    Backend.BackendConfig
      { acceptMap,
        onError,
        onEof
      }
    minDfa
  where
    lexer = Lexer.evalLexerBuilder builder
    lexer' = (\(i, rule) -> rule {Lexer.accept = i}) <$> zip [0 :: Int ..] lexer
    acceptMap = VB.fromList $ Lexer.accept <$> lexer
    nfa = RegexToNfa.lexerToNfa lexer'
    dfa = NfaToDfa.nfaToDfa nfa
    minDfa = Minimize.minimize dfa
