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
import Ilex
import Ilex.Regex qualified as R
import LexerUtils
import LuaRegex
import Test.Tasty

data Token
  = Error
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

data StringQuoteKind = SingleQuote | DoubleQuote

lexMain :: Lex () (Spanned Token)
lexMain = lexMain' =<< getPos

lexMain' :: Pos -> Lex () (Spanned Token)
lexMain' start =
  $( ilex do
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

       -- numbers
       rNumber ~= [|spanned $ Number . inputText|]
       R.cat ["0x", R.some rHexDigit] ~= [|spanned $ Number . inputText|]

       OnAny ~=! [|tok Error|]
       OnEof ~=! [|tok Eof|]
   )
  where
    tok = spanned . const
    spanned f i = do
      end <- getPos
      pure $! Spanned (Span start end) $! f i
    skip = const lexMain

lexComment :: Lex () (Spanned Token)
lexComment =
  $( ilex do
       "\n" ~= [|\_ -> lexMain|]
       R.dot ~= [|\_ -> lexComment|]
       CatchAll ~=! [|\_ -> lexMain|]
   )

lexString :: StringQuoteKind -> String -> Lex () (Either Text Text)
lexString quoteKind = go
  where
    go cs =
      $( ilex do
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
    []
