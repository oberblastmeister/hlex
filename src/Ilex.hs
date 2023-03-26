module Ilex (lex, Lexer.rule) where

import Data.Vector qualified as VB
import Ilex.Internal.Codegen qualified as Codegen
import Ilex.Internal.Lexer qualified as Lexer
import Ilex.Internal.Minimize qualified as Minimize
import Ilex.Internal.NfaToDfa qualified as NfaToDfa
import Ilex.Internal.RegexToNfa qualified as RegexToNfa
import Language.Haskell.TH qualified as TH
import Prelude hiding (lex)
import Debug.Trace

lex :: TH.ExpQ -> TH.ExpQ -> Lexer.LexerBuilder TH.ExpQ () -> TH.ExpQ
lex onError onEof builder =
  Codegen.codegen
    Codegen.CodegenConfig
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
    !_ = traceId $ show minDfa
