{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-splices #-}

module LexerTest (tests) where

import Ilex qualified
import Ilex.Internal.Monad (Lexer)
import Ilex.Regex qualified as RE
import Language.Haskell.TH qualified as TH
import Test.Tasty
import Test.Tasty.HUnit qualified as HU

-- testing :: String
testing :: Lexer () ()
testing =
  $( Ilex.lex [|undefined|] [|undefined|] $ do
       Ilex.rule (RE.string "ab") [|undefined|]
       Ilex.rule (RE.string "abc") [|undefined|]
   )

-- Ilex.token "a" $ do
--   Ilex.char 'a'
--   Ilex.char 'b'
--   Ilex.char 'c'
-- Ilex.token "b" $ do
--   Ilex.char 'a'
--   Ilex.char 'b'
--   Ilex.char 'c'
-- Ilex.token "c" $ do
--   Ilex.char 'a'
--   Ilex.char 'b'
--   Ilex.char 'c'
tests :: TestTree
tests =
  testGroup
    "LexerTest"
    [HU.testCase "testing" $ pure ()]
