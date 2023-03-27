{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-ddump-splices
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}

module LexerTest where

import Data.Text (Text)
import Ilex ((~=))
import Ilex qualified
import Ilex.Internal.Monad (Lex, LexerInput, getCharPos, runLexText)
import Ilex.Regex qualified as RE
import Test.Tasty
import Test.Tasty.HUnit qualified as HU

data Token
  = Forall
  | Let
  | Rec
  | In
  | If
  | Then
  | Else
  | TTrue
  | TFalse
  | Arrow
  | Eq
  | EqEq
  | Neq
  | Lam
  | Plus
  | Minus
  | Mul
  | Div
  | Lparen
  | Rparen
  | Dot
  | Error
  | Eof
  | Num !Text
  | Ident !Text
  deriving (Show, Eq)

tok :: Token -> LexerInput -> Lex () Token
tok = const . pure

lexer :: Lex () Token
lexer = do
  $( Ilex.lex [|tok Error|] [|tok Eof|] $ do
       "forall" ~= [|tok Forall|]
       "if" ~= [|tok If|]
       "let" ~= [|tok Let|]
       "rec" ~= [|tok Rec|]
       "in" ~= [|tok In|]
       "if" ~= [|tok If|]
       "then" ~= [|tok Then|]
       "else" ~= [|tok Else|]
       "true" ~= [|tok TTrue|]
       "false" ~= [|tok TFalse|]
       "->" ~= [|tok Arrow|]
       "=" ~= [|tok Eq|]
       "==" ~= [|tok EqEq|]
       "/=" ~= [|tok Neq|]
       "\\" ~= [|tok Lam|]
       "+" ~= [|tok Plus|]
       "-" ~= [|tok Minus|]
       "*" ~= [|tok Mul|]
       "/" ~= [|tok Div|]
       "(" ~= [|tok Lparen|]
       ")" ~= [|tok Rparen|]
       "." ~= [|tok Dot|]
       RE.some RE.num ~= [|pure . Num . Ilex.inputText|]
       RE.some RE.alpha ~= [|pure . Ident . Ilex.inputText|]
       RE.cat ["--", RE.many RE.dot] ~= [|const lexer|]
       RE.isSpace ~= [|const lexer|]
   )

data Span = Span
  { start :: !Int,
    end :: !Int
  }
  deriving (Show, Eq)

data Spanned a = Spanned
  { span :: {-# UNPACK #-} !Span,
    value :: a
  }
  deriving (Show, Eq)

lexAll :: Lex () [Spanned Token]
lexAll = do
  start <- getCharPos
  t <- lexer
  end <- getCharPos
  case t of
    Eof -> pure [Spanned (Span start end) Eof]
    _ -> do
      ts <- lexAll
      pure $ Spanned (Span start end) t : ts

tests :: TestTree
tests =
  testGroup
    "LexerTest"
    [ HU.testCase "testing" $ do
        let ((), ts) = runLexText "let bruh = 1234 in True" () lexAll
        print ts
        pure ()
    ]
