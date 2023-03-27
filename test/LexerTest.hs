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
import Ilex ((~=), (~>))
import Ilex qualified
import Ilex.Internal.Monad (Lex, getCharPos, runLexText)
import Ilex.Regex (Regex)
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

lexer :: Lex () Token
lexer = do
  $( Ilex.lex [|pure Error|] [|pure Eof|] $ do
       "forall" ~= [|pure Forall|]
       "if" ~= [|pure If|]
       "let" ~= [|pure Let|]
       "rec" ~= [|pure Rec|]
       "in" ~= [|pure In|]
       "if" ~= [|pure If|]
       "then" ~= [|pure Then|]
       "else" ~= [|pure Else|]
       "true" ~= [|pure TTrue|]
       "false" ~= [|pure TFalse|]
       "->" ~= [|pure Arrow|]
       "=" ~= [|pure Eq|]
       "==" ~= [|pure EqEq|]
       "/=" ~= [|pure Neq|]

       "\\" ~= [|pure Lam|]

      --  "+" ~= [|pure Plus|]
       "-" ~= [|pure Minus|]
       "*" ~= [|pure Mul|]
       "/" ~= [|pure Div|]
       "(" ~= [|pure Lparen|]
       ")" ~= [|pure Rparen|]
       "." ~= [|pure Dot|]
       RE.some RE.num ~> [|\i -> pure $ Num $ Ilex.inputText i|]
       RE.some RE.alpha ~> [|\i -> pure $ Ident $ Ilex.inputText i|]
       RE.cat ["--", RE.many RE.dot] ~= [|lexer|]
       RE.isSpace ~= [|lexer|]
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
