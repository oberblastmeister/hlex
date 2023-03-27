{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-splices #-}

module SimpleTest where

import Ilex (tok, (~=))
import Ilex qualified
import Ilex.Internal.Monad (Lex, LexerInput)
import Test.Tasty


testing :: Lex () Int
testing =
  $( Ilex.ilex [|tok 0|] [|tok 1|] $ do
       "abc" ~= [|tok 3|]
       "ab" ~= [|tok 2|]
   )

tests :: TestTree
tests = testGroup "SimpleTest" []
