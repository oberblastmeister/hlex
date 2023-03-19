{-# LANGUAGE TemplateHaskell #-}

module RegexTest where

import Data.ByteString qualified as B
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Text.HLex.Internal.Dfa qualified as Dfa
import Text.HLex.Internal.Minimize qualified as Minimize
import Text.HLex.Internal.Nfa qualified as Nfa
import Text.HLex.Internal.NfaToDfa qualified as NfaToDfa
import Text.HLex.Internal.Regex qualified as RE
import Text.HLex.Internal.RegexToNfa qualified as RegexToNfa
import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Text.HLex.Internal.CharSet qualified as CharSet
import Text.HLex.Internal.Regex


genRegexWith :: Gen Char -> Gen Regex
genRegexWith genChar = go
  where
    go = do
      Gen.recursive
        Gen.choice
        [ Set . CharSet.fromString <$> Gen.list (Range.linear 0 10) genChar
        ]
        [ Gen.subterm2 go go Cat,
          Gen.subterm2 go go Alt,
          Gen.subterm go Rep
        ]

prop_match_string :: Property
prop_match_string = do
  property do
    let genString = Gen.list (Range.linear 0 10) Gen.alphaNum
    s1 <- forAll genString
    s2 <- forAll genString
    accept <- forAll $ Gen.int (Range.linear 0 100)
    let r = RE.string s1
    let nfa = RegexToNfa.regexToNfa accept r
    let dfa' = NfaToDfa.nfaToDfa nfa
    let dfa = Minimize.minimize dfa'
    annotateShow r
    annotateShow nfa
    annotateShow dfa'
    annotateShow dfa
    let bs1 = T.encodeUtf8 $ T.pack s1
    let bs2 = T.encodeUtf8 $ T.pack s2
    let bs = bs1 <> bs2
    let dfaRes = Dfa.simulate bs dfa
    let nfaRes = Nfa.simulate bs nfa
    dfaRes === nfaRes
    dfaRes === Just (B.length bs1, accept)
    pure ()

prop_valid :: Property
prop_valid = property do
  r <- forAll $ genRegexWith Gen.unicode
  let nfa = RegexToNfa.regexToNfa () r 
  assert $ Nfa.valid nfa
  let dfa = NfaToDfa.nfaToDfa nfa
  assert $ Dfa.valid dfa
  let minDfa = Minimize.minimize dfa
  assert $ Dfa.valid minDfa

tests :: TestTree
tests =
  testGroup
    "RegexTest"
    [fromGroup $$(discover)]

-- [ testProperty "valid" $ property do
--     undefined
-- ]
