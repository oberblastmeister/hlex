{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-ddump-splices
-dsuppress-coercions
-dsuppress-idinfo #-}

module SimpleTest where

import Data.Text (Text)
import Hlex
import Hlex.Regex qualified as R
import LexerUtils (lexUntil)
import Test.Tasty
import TestUtils (testGoldenInShow)

testing :: Lex () Int
testing =
  $( hlex do
       OnAny ~=! [|tok 0|]
       OnEof ~=! [|tok 1|]
       "abc" ~= [|tok 3|]
       "ab" ~=? ([|tok 4|], [|\_ _ -> True|])
       "ab" ~=? ([|tok 2|], [|\_ _ -> True|])
   )
  where
    tok = const . pure

prefix :: Lex () [Text]
prefix =
  lexUntil (== "eof") $
    $( hlex do
         "a" ~= [|pure . inputText|]
         R.cat [R.some "a", "b"] ~= [|pure . inputText|]
         OnAny ~=! [|\_ -> pure "other"|]
         OnEof ~=! [|\_ -> pure "eof"|]
     )

tests :: TestTree
tests =
  testGroup
    "SimpleTest"
    [ golden "prefix" do
        let ((), ts) = lexText prefix "aaaaaaaaaaaaaaab" ()
        let ((), ts') = lexText prefix "a" ()
        let ((), ts'') = lexText prefix "aaa" ()
        pure $ ts ++ ts' ++ ts''
    ]

golden :: Show a => String -> IO a -> TestTree
golden = testGoldenInShow "SimpleTest"
