{-# LANGUAGE TemplateHaskell #-}

module RegexTest (tests) where

import Data.ByteString qualified as B
import Data.Char qualified as Char
import Data.Foldable (for_)
import Data.IntSet qualified as IntSet
import Data.Maybe qualified as Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Vector qualified as VB
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Text.HLex.Internal.CharSet qualified as CharSet
import Text.HLex.Internal.Dfa qualified as Dfa
import Text.HLex.Internal.Minimize qualified as Minimize
import Text.HLex.Internal.Nfa qualified as Nfa
import Text.HLex.Internal.NfaToDfa qualified as NfaToDfa
import Text.HLex.Internal.Regex
import Text.HLex.Internal.Regex qualified as RE
import Text.HLex.Internal.RegexToNfa qualified as RegexToNfa
import Text.HLex.Internal.Utf8
import Text.HLex.Internal.Utils

genRegexWith :: Gen Char -> Gen Regex
genRegexWith genChar = go
  where
    go = do
      Gen.recursive
        Gen.choice
        [ pure Empty,
          Set . CharSet.fromString <$> Gen.list (Range.linear 0 10) genChar
        ]
        [ Gen.subterm2 go go Cat,
          Gen.subterm2 go go Alt,
          Gen.subterm go Rep
        ]

prop_match_string :: Property
prop_match_string = property do
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

prop_minimize :: Property
prop_minimize = property do
  r <- forAll $ genRegexWith Gen.unicode
  let nfa = RegexToNfa.regexToNfa () r
  let dfa = NfaToDfa.nfaToDfa nfa
  let equivs = Minimize.dfaEquivalentStates dfa
  assert $ Minimize.isMinimal equivs dfa

predicatesProperty :: (Char -> Bool) -> Property
predicatesProperty pred = property do
  let r = RE.when pred
  let nfa = RegexToNfa.regexToNfa () r
  let dfa = NfaToDfa.nfaToDfa nfa
  let minDfa = Minimize.minimize dfa
  let ranges = CharSet.fromPred pred
  let cs = CharSet.toList ranges
  c <- forAll $ Gen.element cs
  let transitionLessStates =
        filter (\(i, state) -> null (Nfa.transitions state) && IntSet.null (Nfa.emptyTransitions state)) $
          zip [0 :: Int ..] $
            VB.toList (Nfa.states nfa)
  let transitionLessStates' =
        filter (\(i, state) -> null (Nfa.transitions state) && not (IntSet.null (Nfa.emptyTransitions state))) $
          zip [0 :: Int ..] $
            VB.toList (Nfa.states nfa)
  let acceptingStates = filter (Maybe.isJust . Dfa.accept) $ VB.toList (Dfa.states minDfa)
  annotateShow $ length $ CharSet.toRangeList ranges
  annotateShow $ length acceptingStates
  annotateShow transitionLessStates
  -- annotateShow transitionLessStates'
  -- annotateShow ranges
  -- annotateShow c
  let bs = T.encodeUtf8 $ T.singleton c
  annotateShow bs
  let nfaRes = Nfa.simulate bs nfa
  let dfaRes = Dfa.simulate bs minDfa
  nfaRes === dfaRes
  let sequences = utf8Sequences . toScalarRange =<< rangeSetToIntervalList (CharSet.unCharSet ranges)
  annotateShow $ isSurrogate c
  -- annotateShow sequences
  assert $ any (`matchUtf8Sequence` B.unpack bs) sequences
  -- nfaRes === Right (B.length bs, ())
  nfaRes === Just (B.length bs, ())

isSurrogate :: Char -> Bool
isSurrogate c =
  (0xD800 <= x && x <= 0xDBFF)
    || (0xDC00 <= x && x <= 0xDFFF)
  where
    x = Char.ord c

tests :: TestTree
tests =
  testGroup
    "RegexTest"
    [ fromGroup $$(discover),
      testProperty "isSpace" $ predicatesProperty Char.isSpace,
      testProperty "isAlpha" $ predicatesProperty Char.isAlpha,
      testProperty "all" $ predicatesProperty (const True)
    ]

-- [ testProperty "valid" $ property do
--     undefined
-- ]
