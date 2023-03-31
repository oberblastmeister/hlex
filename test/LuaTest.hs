{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-dsuppress-coercions
-dsuppress-idinfo #-}

module LuaTest where

import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Hlex
import Hlex.Regex qualified as R
import LexerUtils
import LuaRegex
import Test.Tasty
import Test.Tasty.HUnit
import TestUtils (testGoldenInShow)

data Token
  = Error !Text
  | Eof
  | -- operators
    Plus
  | Minus
  | Star
  | Slash
  | Percent
  | Caret
  | Hash
  | EqEq
  | TildeEq
  | Lt
  | LtEq
  | Gt
  | GtEq
  | Semicolon
  | Colon
  | Comma
  | Dot
  | DotDot
  | DotDotDot
  | Eq
  | -- delimiters
    LParen
  | RParen
  | LBrace
  | RBrace
  | LBracket
  | RBracket
  | -- keywords
    And
  | Break
  | Do
  | Else
  | ElseIf
  | End
  | TFalse
  | For
  | Function
  | Goto
  | If
  | In
  | Local
  | Nil
  | Not
  | Or
  | Repeat
  | Return
  | Then
  | TTrue
  | Until
  | While
  | -- literals
    Str !Text
  | Number !Text
  | -- name
    Name !Text
  deriving (Show, Eq)

data StringQuoteKind = SingleQuote | DoubleQuote

lexAll :: Lex () [Spanned Token]
lexAll = lexUntil ((== Eof) . value) lexMain

lexMain :: Lex () (Spanned Token)
lexMain = lexMain' =<< getPos

lexMain' :: Pos -> Lex () (Spanned Token)
lexMain' start =
  $( hlex do
       rWhitespace ~= [|skip|]

       "+" ~= [|tok Plus|]
       "-" ~= [|tok Minus|]
       "*" ~= [|tok Star|]
       "/" ~= [|tok Slash|]
       "%" ~= [|tok Percent|]
       "^" ~= [|tok Caret|]
       "#" ~= [|tok Hash|]
       "==" ~= [|tok EqEq|]
       "~=" ~= [|tok TildeEq|]
       "<" ~= [|tok Lt|]
       "<=" ~= [|tok LtEq|]
       ">" ~= [|tok Gt|]
       ">=" ~= [|tok GtEq|]
       ";" ~= [|tok Semicolon|]
       ":" ~= [|tok Colon|]
       "," ~= [|tok Comma|]
       "." ~= [|tok Dot|]
       ".." ~= [|tok DotDot|]
       "..." ~= [|tok DotDotDot|]
       "=" ~= [|tok Eq|]

       -- delimiters
       "(" ~= [|tok LParen|]
       ")" ~= [|tok RParen|]
       "{" ~= [|tok LBrace|]
       "}" ~= [|tok RBrace|]
       "[" ~= [|tok LBracket|]
       "]" ~= [|tok RBracket|]

       -- keywords
       "and" ~= [|tok And|]
       "break" ~= [|tok Break|]
       "do" ~= [|tok Do|]
       "else" ~= [|tok Else|]
       "elseif" ~= [|tok ElseIf|]
       "end" ~= [|tok End|]
       "false" ~= [|tok TFalse|]
       "for" ~= [|tok For|]
       "function" ~= [|tok Function|]
       "goto" ~= [|tok Goto|]
       "if" ~= [|tok If|]
       "in" ~= [|tok In|]
       "local" ~= [|tok Local|]
       "nil" ~= [|tok Nil|]
       "not" ~= [|tok Not|]
       "or" ~= [|tok Or|]
       "repeat" ~= [|tok Repeat|]
       "return" ~= [|tok Return|]
       "then" ~= [|tok Then|]
       "true" ~= [|tok TTrue|]
       "until" ~= [|tok Until|]
       "while" ~= [|tok While|]

       "\"" ~= [|withQuote DoubleQuote|]
       "'" ~= [|withQuote SingleQuote|]

       -- names
       R.cat [rVarStart, R.many rVarContinue] ~= [|spanned $ Name . inputText|]

       -- numbers
       rNumber ~= [|spanned $ Number . inputText|]
       R.cat ["0x", R.some rHexDigit] ~= [|spanned $ Number . inputText|]

       OnAny ~=! [|tok $ Error "unknown"|]
       OnEof ~=! [|tok Eof|]
   )
  where
    tok = spanned . const
    withQuote quote _ = do
      end <- getPos
      res <- lexString quote ""
      pure $ Spanned (Span start end) case res of
        Left e -> Error e
        Right s -> Str s
    spanned f i = do
      end <- getPos
      pure $! Spanned (Span start end) $! f i
    skip = const lexMain

lexComment :: Lex () (Spanned Token)
lexComment =
  $( hlex do
       "\n" ~= [|\_ -> lexMain|]
       R.dot ~= [|\_ -> lexComment|]
       CatchAll ~=! [|\_ -> lexMain|]
   )

lexString :: StringQuoteKind -> String -> Lex () (Either Text Text)
lexString quoteKind = go
  where
    go cs =
      $( hlex do
           "\""
             ~= [|
               \_ -> case quoteKind of
                 DoubleQuote -> pure $ Right $ T.reverse $ T.pack cs
                 SingleQuote -> go $ '"' : cs
               |]
           "'"
             ~= [|
               \_ -> case quoteKind of
                 SingleQuote -> pure $ Right $ T.reverse $ T.pack cs
                 DoubleQuote -> go $ '\'' : cs
               |]
           "\\a" ~= [|addChar '\a'|]
           "\\b" ~= [|addChar '\b'|]
           "\\f" ~= [|addChar '\f'|]
           "\\n" ~= [|addChar '\n'|]
           "\\r" ~= [|addChar '\r'|]
           "\\t" ~= [|addChar '\t'|]
           "\\v" ~= [|addChar '\v'|]
           "\\\\" ~= [|addChar '\\'|]
           "\\\"" ~= [|addChar '"'|]
           "\\'" ~= [|addChar '\''|]
           R.cat ["\\", R.dot] ~= [|\i -> pure $ Left $ "invalid escape sequence: " <> inputText i|]
           OnAny ~=! [|\i -> go $ fst (Maybe.fromJust (T.uncons (inputText i))) : cs|]
           OnEof ~=! [|\_ -> pure $ Left "unclosed string"|]
       )
      where
        addChar c = const $ go (c : cs)

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

    check :: (Show a, Eq a, HasCallStack) => Lex () a -> Text -> a -> Assertion
    check lex text actual = do
      let ((), ts) = lexText lex text ()
      ts @?= actual

golden :: Show a => String -> IO a -> TestTree
golden = testGoldenInShow "LuaTest"
