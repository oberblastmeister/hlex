{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-ddump-splices
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

lexer :: Lex () (Spanned Token)
lexer = do
  $( ilex [|tok $ Error "unknown"|] [|tok Eof|] $ do
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
       R.cat [rLower, R.many rIdent] ~= [|spanned $ Ident . Ilex.inputText|]
       R.cat [rUpper, R.many rIdent] ~= [|spanned $ ConIdent . Ilex.inputText|]
       R.some R.digit ~= [|spanned $ Num . Ilex.inputText|]
       R.cat ["--", R.many R.dot] ~= [|comment|]
       R.cat ["{-", R.many $ R.alt [R.dot, "\n"], "-}"] ~= [|comment|]
       R.some R.isSpace ~= [|skip|]
       R.some "\n" ~= [|skip|]
       "\""
         ~= [|
           \i -> do
             let start = inputStart i
             res <- Except.runExceptT $ lexString ['"']
             end <- getPos
             pure $ Spanned (Span start end) $ case res of
               Left e -> Error e
               Right x -> String x
           |]
   )
  where
    lexString cs =
      $( ilex [|\_ -> Except.throwError "unsupposed string character"|] [|\_ -> Except.throwError "unclosed string"|] $ do
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
       )
      where
        addChar c _i = lexString $ c : cs
    skip = const lexer
    comment = spanned $ Comment . inputText

lexAll :: Lex () [Spanned Token]
lexAll = do
  t <- lexer
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
