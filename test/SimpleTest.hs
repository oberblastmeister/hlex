{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-ddump-splices
-dsuppress-coercions
-dsuppress-idinfo #-}

module SimpleTest where

import Ilex
import LexerUtils
import Test.Tasty

testing :: Lex () Int
testing =
  $( ilex [|tok 0|] [|tok 1|] $ do
       "abc" ~= [|tok 3|]
       "ab" ~=? ([|tok 4|], [|\_ _ -> True|])
       "ab" ~=? ([|tok 2|], [|\_ _ -> True|])
   )
  where
    tok = const . pure

tests :: TestTree
tests = testGroup "SimpleTest" []
