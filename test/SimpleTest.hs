{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -ddump-simpl
-ddump-to-file
-dsuppress-module-prefixes
-ddump-splices
-dsuppress-coercions
-dsuppress-idinfo #-}

module SimpleTest where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Vector qualified as VB
import Hlex
import Hlex.Internal.Dfa qualified as Dfa
import Hlex.Internal.Driver (rulesToDfa)
import Hlex.Internal.Rule qualified as Rule
import Hlex.Regex qualified as R
import LexerUtils (lexUntil)
import Test.Tasty
import Test.Tasty.HUnit
import TestUtils (testGoldenInShow)
import Text.Pretty.Simple (pPrint)

testing :: Lex () Int
testing =
  $( hlex do
       "ab" ~=? ([|tok 4|], [|\_ _ -> False|])
       "ab" ~=? ([|tok 2|], [|\_ _ -> False|])
       OnAny ~=! [|tok 0|]
       OnEof ~=! [|tok 1|]
       "abc" ~= [|tok 3|]
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

shrinkAccepts :: NonEmpty (Rule.Accept' a) -> NonEmpty (Rule.Accept' a)
shrinkAccepts (a NE.:| as)
  | Rule.Accept {context = Nothing} <- a = a NE.:| []
  | otherwise = a NE.:| foldr f as []
  where
    f a@Rule.Accept {context = Nothing} _as = [a]
    f a as = a : as

singlePred :: IO ()
singlePred = do
  let dfa = rulesToDfa $ Rule.evalRuleBuilder $ do
        "a" ~=? ((1 :: Int), (3 :: Int))
        CatchAll ~=! (2 :: Int)
  dfa <- pure $ shrinkAccepts <$> dfa
  let acceptMap = VB.fromList $ fmap Rule.exp $ concatMap NE.toList $ Dfa.accepts dfa
  pure ()

tests :: TestTree
tests =
  testGroup
    "SimpleTest"
    [ testCase "simplePred" do
        singlePred,
      golden "prefix" do
        let ((), ts) = lexText prefix "aaaaaaaaaaaaaaab" ()
        let ((), ts') = lexText prefix "a" ()
        let ((), ts'') = lexText prefix "aaa" ()
        pure $ ts ++ ts' ++ ts''
    ]

golden :: Show a => String -> IO a -> TestTree
golden = testGoldenInShow "SimpleTest"
