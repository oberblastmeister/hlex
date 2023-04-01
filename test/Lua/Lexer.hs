{-# LANGUAGE TemplateHaskell #-}

module Lua.Lexer where

import Data.Text qualified as T
import Hlex
import Hlex.Regex qualified as R
import LexerUtils
import Lua.Lexer.Monad
import Lua.Lexer.Regex
import Lua.Lexer.String
import Lua.Token

lexAll :: M [Spanned Token]
lexAll = lexUntil ((== Eof) . value) lexMain

lexMain :: M (Spanned Token)
lexMain = lexMain' =<< getPos

lexMain' :: Pos -> M (Spanned Token)
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
       "[" ~= [|goBracket|]
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

       "--" ~= [|\_ -> lexEnterComment start|]

       -- names
       R.cat [rVarStart, R.many rVarContinue] ~= [|spanned $ Name . inputText|]

       -- numbers
       rNumber ~= [|spanned $ Number . inputText|]
       R.cat ["0x", R.some rHexDigit] ~= [|spanned $ Number . inputText|]

       OnAny ~=! [|tok $ Error Unknown|]
       OnEof ~=! [|tok Eof|]
   )
  where
    tok = spanned . const
    goBracket i = do
      lexEnterLongString
        start
        ( \(numEqs, i') -> do
            end <- getPos
            let toDrop = 2 + numEqs
            let text = T.drop toDrop $ T.dropEnd toDrop $ inputText $ spanInput i i'
            pure $ Spanned (Span start end) $ Str text
        )
        ( do
            end <- getPos
            pure $ Spanned (Span start end) LBracket
        )
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

lexEnterComment :: Pos -> M (Spanned Token)
lexEnterComment start =
  $( hlex do
       "[" ~= [|\_ -> lexEnterLongString start (const lexMain) lexMain|]
       CatchAll ~=! [|\_ -> lexComment|]
   )

lexComment :: M (Spanned Token)
lexComment =
  $( hlex do
       R.dot ~= [|\_ -> lexComment|]
       CatchAll ~=! [|\_ -> lexMain|]
   )
