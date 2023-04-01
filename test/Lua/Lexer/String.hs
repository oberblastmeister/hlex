{-# LANGUAGE TemplateHaskell #-}

module Lua.Lexer.String
  ( lexString,
    lexEnterLongString,
    StringQuoteKind (..),
  )
where

import Data.Maybe qualified as Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Hlex
import Hlex.Regex qualified as R
import LexerUtils
import Lua.Lexer.Monad
import Lua.Token

type LongStringM = M (Either TokenError (Int, Utf8Input))

lexEnterLongString :: Pos -> ((Int, Utf8Input) -> M (Spanned Token)) -> M (Spanned Token) -> M (Spanned Token)
lexEnterLongString start f other = do
  prevPos <- getPos
  $( hlex do
       "[" ~= [|\_ -> lexLongString 0 >>= done|]
       "=" ~= [|\_ -> lexLongStringBracketLeft 1 >>= done|]
       CatchAll ~=! [|\_ -> setPos prevPos >> other|]
   )
  where
    done res = do
      end <- getPos
      case res of
        Left e -> pure $ Spanned (Span start end) $ Error e
        Right i -> f i

data StringQuoteKind = SingleQuote | DoubleQuote

lexString :: StringQuoteKind -> String -> M (Either TokenError Text)
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
           R.cat ["\\", R.dot] ~= [|\i -> pure $ Left $ InvalidEscape $ inputText i|]
           OnAny ~=! [|\i -> go $ fst (Maybe.fromJust (T.uncons (inputText i))) : cs|]
           OnEof ~=! [|\_ -> pure $ Left UnterminatedString|]
       )
      where
        addChar c = const $ go (c : cs)

lexLongStringBracketLeft :: Int -> LongStringM
lexLongStringBracketLeft !openingEqs =
  $( hlex do
       "=" ~= [|\_ -> lexLongStringBracketLeft $! openingEqs + 1|]
       "[" ~= [|\_ -> lexLongString openingEqs|]
       OnAny ~=! [|pure . Left . InvalidCharacter . inputText|]
       OnEof ~=! [|\_ -> pure $ Left UnterminatedString|]
   )

lexLongString :: Int -> LongStringM
lexLongString !openingEqs =
  $( hlex do
       "]" ~= [|\_ -> lexLongStringBracketRight openingEqs|]
       OnAny ~=! [|\_ -> lexLongString openingEqs|]
       OnEof ~=! [|\_ -> pure $ Left UnterminatedString|]
   )

lexLongStringBracketRight :: Int -> LongStringM
lexLongStringBracketRight openingEqs = go 0
  where
    go :: Int -> LongStringM
    go !closingEqs =
      $( hlex do
           "=" ~= [|\_ -> go $! closingEqs + 1|]
           "]" ~= [|goClose|]
           OnAny ~=! [|pure . Left . InvalidCharacter . inputText|]
           OnEof ~=! [|\_ -> pure $ Left UnterminatedString|]
       )
      where
        goClose i =
          if openingEqs == closingEqs
            then pure $ Right (openingEqs, i)
            else pure $ Left $ InvalidEqs openingEqs closingEqs
