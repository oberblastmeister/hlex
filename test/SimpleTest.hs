{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-splices #-}

module SimpleTest where

import Ilex ((~=))
import Ilex qualified
import Ilex.Internal.Monad (Lex)
import Test.Tasty

testing :: Lex () Int
testing =
  $( Ilex.lex [|pure 0|] [|pure 1|] $ do
       "abc" ~= [|pure 3|]

       "ab" ~= [|pure 2|]
   )

tests :: TestTree
tests = testGroup "SimpleTest" []
