{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}

module LexerTest where

import Control.Monad.Except qualified as Except
import Data.Text (Text)
import Data.Text qualified as T
import Ilex
import Ilex.Regex qualified as R
import LexerUtils
import Test.Tasty
import TestUtils

data Token
  = Forall
  | Let
  | Rec
  | In
  | If
  | Then
  | Else
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
  | Error !Text
  | Eof
  | Num !Text
  | Ident !Text
  | ConIdent !Text
  | Comment !Text
  | String !Text
  deriving (Show, Eq)

lexer :: Pos -> Lex () (Spanned Token)
lexer start =
  $( ilex do
       "forall" ~= [|tok Forall|]
       "if" ~= [|tok If|]
       "let" ~= [|tok Let|]
       "rec" ~= [|tok Rec|]
       "in" ~= [|tok In|]
       "if" ~= [|tok If|]
       "then" ~= [|tok Then|]
       "else" ~= [|tok Else|]
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
       R.cat [rLower, R.many rIdent] ~= [|spanned $ Ident . inputText|]
       R.cat [rUpper, R.many rIdent] ~= [|spanned $ ConIdent . inputText|]
       R.some R.digit ~= [|spanned $ Num . inputText|]
       R.cat ["--", R.many R.dot] ~= [|comment|]
       R.cat ["{-", R.many $ R.alt [R.dot, "\n"], "-}"] ~= [|comment|]
       R.some R.isSpace ~= [|skip|]
       R.some "\n" ~= [|skip|]
       "\""
         ~= [|
           \_ -> do
             res <- Except.runExceptT $ lexString ['"']
             end <- getPos
             pure $ Spanned (Span start end) case res of
               Left e -> Error e
               Right x -> String x
           |]
       OnAny ~=! [|tok $ Error "unknown"|]
       OnEof ~=! [|tok Eof|]
   )
  where
    tok = spanned . const
    spanned f i = do
      end <- getPos
      pure $! Spanned (Span start end) $! f i
    skip = const $ lexer =<< getPos
    comment = spanned $ Comment . inputText
    lexString cs =
      $( ilex do
           "\"" ~= [|\_ -> pure $! T.pack $ reverse $ '"' : cs|]
           "\\n" ~= [|addChar '\n'|]
           "\\t" ~= [|addChar '\t'|]
           "\\\\" ~= [|addChar '\\'|]
           "\\a" ~= [|addChar '\a'|]
           R.cat ["\\", R.dot] ~= [|\_ -> Except.throwError "invalid escape sequence"|]
           R.dot
             ~= [|
               \i -> do
                 let t = Ilex.inputText i
                 case T.uncons t of
                   Nothing -> error "impossible"
                   Just (c, _) -> lexString $ c : cs
               |]
           OnAny ~=! [|\_ -> Except.throwError "unsupposed string character"|]
           OnEof ~=! [|\_ -> Except.throwError "unclosed string"|]
       )
      where
        addChar c _i = lexString $ c : cs

lexAll :: Lex () [Spanned Token]
lexAll = do
  t <- lexer =<< getPos
  case value t of
    Eof -> pure [t]
    _ -> do
      ts <- lexAll
      pure $ t : ts

tests :: TestTree
tests =
  testGroup
    "LexerTest"
    [ golden "smoke" do
        let ((), ts) = lexText lexAll "let {-😀-}  bru234h \n=\n1234 in True" () 
        pure ts,
      golden "strings" do
        let ((), ts) = lexText lexAll "let str = \"🤣\\aa\\ts\\ndf\" in str" ()
        pure ts,
      golden "invalid_escapes" do
        let ((), ts) = lexText lexAll "\"\\z\\y\\u\" ." ()
        pure ts
    ]

golden :: Show a => String -> IO a -> TestTree
golden = testGoldenInShow "LexerTest"
