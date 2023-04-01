{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}

module LuaTest where

import Data.Text (Text)
import Data.Text qualified as T
import Hlex (lexText)
import LexerUtils
import Lua.Lexer
import Lua.Lexer.Monad
import Lua.Token
import Test.Tasty
import Test.Tasty.HUnit
import TestUtils (testGoldenInShow)

tests :: TestTree
tests =
  testGroup
    "Lua"
    [ testCase "numbers" do
        let ns = ["3", "3.0", "3.1416", "314.16e-2", "0.31416E1", "0xff", "0x56"]
        check' (T.intercalate " " ns) (fmap Number ns ++ [Eof]),
      testCase "names" do
        check' "b yo" [Name "b", Name "yo", Eof],
      testCase "strings" do
        check' "\"hello\" 'world'" [Str "hello", Str "world", Eof],
      testCase "long string" do
        check'
          "[[asdf]]   [=[\\n\\a\n\n\\r[]=] [==[er2323=3]==]"
          [Str "asdf", Str "\\n\\a\n\n\\r[", Str "er2323=3", Eof],
      testCase "long string error" do
        check' "[=[]==] [==[]=]" [Error (InvalidEqs 1 2), Error (InvalidEqs 2 1), Eof],
      testCase "keywords" do
        let s =
              "+ - * / % ^ # == ~= <= >= < > = ( ) { } [ ]\
              \ ; : , . .. ... and break do else elseif end\
              \ false for function if in local nil not or repeat\
              \ return then true until while n"
        check'
          s
          [ Plus,
            Minus,
            Star,
            Slash,
            Percent,
            Caret,
            Hash,
            EqEq,
            TildeEq,
            LtEq,
            GtEq,
            Lt,
            Gt,
            Eq,
            LParen,
            RParen,
            LBrace,
            RBrace,
            LBracket,
            RBracket,
            Semicolon,
            Colon,
            Comma,
            Dot,
            DotDot,
            DotDotDot,
            And,
            Break,
            Do,
            Else,
            ElseIf,
            End,
            TFalse,
            For,
            Function,
            If,
            In,
            Local,
            Nil,
            Not,
            Or,
            Repeat,
            Return,
            Then,
            TTrue,
            Until,
            While,
            Name "n",
            Eof
          ]
    ]
  where
    check' :: Text -> [Token] -> Assertion
    check' = check ((fmap . fmap) value lexAll)

    check :: (Show a, Eq a, HasCallStack) => M a -> Text -> a -> Assertion
    check lex text actual = do
      let ((), ts) = lexText lex text ()
      ts @?= actual

golden :: (Show a) => String -> IO a -> TestTree
golden = testGoldenInShow "LuaTest"
