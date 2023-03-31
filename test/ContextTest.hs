{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-ddump-splices
-dsuppress-coercions
-dsuppress-idinfo #-}

module ContextTest where

import Hlex
import LexerUtils (lexUntil)
import Test.Tasty
import TestUtils (testGoldenInShow)

lexer :: Int -> Lex () Int
lexer n =
  $( hlex do
       "ab" ~=? ([|tok 5|], [|\_ i -> $(matches "zz") i|])
       "ab" ~=? ([|tok 2|], [|\_ _ -> n == 0|])
       "ab" ~=? ([|tok 3|], [|\_ _ -> n == 1|])
       "ab" ~=? ([|tok 4|], [|\_ _ -> n == 2|])
       OnAny ~=! [|tok 0|]
       OnEof ~=! [|tok 1|]
   )
  where
    tok = const . pure

lexAll :: Int -> Lex () [Int]
lexAll = lexUntil (== 1) . lexer

tests :: TestTree
tests =
  testGroup
    "ContextTest"
    [ golden "smoke" do
        let ((), ts) = lexText (lexAll 0) "abab" ()
        let ((), ts') = lexText (lexAll 1) "abab" ()
        let ((), ts'') = lexText (lexAll 2) "abzzab" ()
        let ((), ts''') = lexText (lexAll 1234) "abab" ()
        pure $ ts ++ ts' ++ ts'' ++ ts'''
    ]

golden :: Show a => String -> IO a -> TestTree
golden = testGoldenInShow "ContextTest"
