{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}

module SimpleTest where

import Ilex
import LexerUtils
import Test.Tasty

testing :: Lex () (Spanned Int)
testing =
  $( ilex [|tok 0|] [|tok 1|] $ do
       "abc" ~= [|tok 3|]
       "ab" ~= [|tok 2|]
   )

tests :: TestTree
tests = testGroup "SimpleTest" []
