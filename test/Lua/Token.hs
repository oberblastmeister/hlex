module Lua.Token
  ( Token (..),
    TokenError (..),
  )
where

import Data.Text (Text)

data Token
  = Error !TokenError
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

data TokenError
  = Unknown
  | InvalidEqs !Int !Int
  | InvalidEscape !Text
  | InvalidCharacter !Text
  | UnterminatedString
  deriving (Show, Eq)
