{-# LANGUAGE TemplateHaskell #-}

module ContextTest where

import Ilex
import LexerUtils (lexUntil)
import Test.Tasty
import TestUtils (testGoldenInShow)

lexer :: Int -> Lex () Int
lexer n =
  $( ilex [|tok 0|] [|tok 1|] $ do
       "ab" ~=? ([|tok 2|], [|\_ _ -> n == 0|])
       "ab" ~=? ([|tok 5|], [|\_ i -> inputLength i == 3|])
       "ab" ~=? ([|tok 3|], [|\_ _ -> n == 1|])
       "ab" ~=? ([|tok 4|], [|\_ _ -> n == 2|])
   )
  where
    tok = const . pure

lexAll = lexUntil (== 1)

tests :: TestTree
tests =
  testGroup
    "ContextTest"
    [ golden "smoke" do
        let s = "abab"
        let ((), ts) = lexText (lexAll $ lexer 0) s ()
        let ((), ts') = lexText (lexAll $ lexer 1) s ()
        let ((), ts'') = lexText (lexAll $ lexer 2) s ()
        let ((), ts''') = lexText (lexAll $ lexer 1) "abzzz" ()
        pure $ ts ++ ts' ++ ts'' ++ ts'''
    ]

golden :: Show a => String -> IO a -> TestTree
golden = testGoldenInShow "ContextTest"
