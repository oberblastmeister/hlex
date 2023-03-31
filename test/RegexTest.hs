{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}

module RegexTest (tests) where

import Control.Monad (replicateM)
import Data.ByteString qualified as B
import Data.Char qualified as Char
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty
import Test.Tasty.Hedgehog
import Hlex.Internal.CharSet qualified as CharSet
import Hlex.Internal.Dfa qualified as Dfa
import Hlex.Internal.Minimize qualified as Minimize
import Hlex.Internal.Nfa qualified as Nfa
import Hlex.Internal.NfaToDfa qualified as NfaToDfa
import Hlex.Internal.Regex (Regex)
import Hlex.Internal.Regex qualified as RE
import Hlex.Internal.RegexToNfa qualified as RegexToNfa
import Hlex.Internal.Utf8
import Hlex.Internal.Utils
import qualified Data.List.NonEmpty as NE

genRegexWith :: Gen Char -> Gen Regex
genRegexWith genChar = go
  where
    go = do
      Gen.recursive
        Gen.choice
        [ pure RE.Empty,
          RE.Set . CharSet.fromList <$> Gen.list (Range.linear 0 10) genChar
        ]
        [ Gen.subterm2 go go (<>),
          Gen.subterm2 go go (\r r' -> RE.alt [r, r']),
          Gen.subterm go RE.Rep
        ]

genAltRegex :: Int -> String -> Gen String -> Gen Regex
genAltRegex i s genS = RE.alt . fmap RE.string <$> replicateM i ((++ s) <$> genS)

genSuffixRegex :: Gen String -> Gen Regex
genSuffixRegex genS = do
  s <- genS
  ss <- Gen.filter (\ss -> List.nub ss == ss) $ Gen.list (Range.linear 2 10) genS
  s' <- genS
  pure $ RE.cat [RE.alt (RE.string . (++ s) <$> ss), RE.string s']

prop_match_string :: Property
prop_match_string = property do
  let genString = Gen.list (Range.linear 0 10) Gen.unicode
  s1 <- forAll genString
  s2 <- forAll genString
  let t1 = T.pack s1
  accept <- forAll $ Gen.int (Range.linear 0 100)
  let r = RE.string s1
  let nfa = RegexToNfa.regexToNfa accept r
  let dfa' = NE.head <$> NfaToDfa.nfaToDfa nfa
  let dfa = Minimize.minimize dfa'
  annotateShow nfa
  annotateShow dfa
  annotateShow r
  let bs1 = T.encodeUtf8 $ T.pack s1
  let bs2 = T.encodeUtf8 $ T.pack s2
  let bs = bs1 <> bs2
  let dfaRes = Dfa.simulate bs dfa
  let nfaRes = Nfa.simulate bs nfa
  dfaRes === nfaRes
  dfaRes === Just (B.length bs1, T.length t1, accept)
  pure ()

prop_valid :: Property
prop_valid = property do
  r <- forAll $ genRegexWith Gen.unicodeAll
  let nfa = RegexToNfa.regexToNfa () r
  assert $ Nfa.valid nfa
  let dfa = NfaToDfa.nfaToDfa nfa
  assert $ Dfa.valid dfa
  let minDfa = Minimize.minimize dfa
  assert $ Dfa.valid minDfa

prop_minimize :: Property
prop_minimize = property do
  -- cannot include surrogates, because those might turn into Null regexes
  r <- forAll $ genSuffixRegex $ Gen.string (Range.linear 1 10) Gen.unicode
  let nfa = RegexToNfa.regexToNfa () r
  let dfa = NfaToDfa.nfaToDfa nfa
  annotateShow nfa
  annotateShow dfa
  let equivs = Minimize.dfaEquivalentStates dfa
  let merged = sum $ map (\e -> if IntSet.size e > 1 then 1 :: Int else 0) equivs
  annotateShow equivs
  assert $ merged > 1
  assert $ Minimize.isMinimal equivs dfa

predicatesProperty :: (Char -> Bool) -> Property
predicatesProperty pred = property do
  -- FIXME: can't do unicodeAll because T.singleton will convert the surrogate into the unicode replacement character
  c <- forAll $ Gen.choice [Gen.element cs, Gen.unicodeAll]
  annotateShow $ isSurrogate c
  let bs = encodeCharUtf8WithInvalid c
  annotateShow bs
  annotateShow $ bs == encodeCharUtf8WithInvalid '\xfffd'
  let nfaRes = Nfa.simulate bs nfa
  let dfaRes = Dfa.simulate bs minDfa
  assert $ Dfa.valid minDfa
  nfaRes === dfaRes
  let sequences = utf8Sequences . toScalarRange =<< rangeSetToIntervalList (CharSet.unCharSet ranges)
  -- sometimes, the predicates may accept surrogates
  -- however, we still will not accept them
  if not (isSurrogate c) && pred c
    then do
      assert $ any (`matchUtf8Sequence` bs) sequences
      dfaRes === Just (B.length bs, 1, ())
    else do
      assert $ not $ any (`matchUtf8Sequence` bs) sequences
      dfaRes === Nothing
  where
    r = RE.when' pred
    nfa = RegexToNfa.regexToNfa () r
    dfa = NE.head <$> NfaToDfa.nfaToDfa nfa
    minDfa = Minimize.minimize dfa
    ranges = CharSet.fromPred pred
    cs = CharSet.toList ranges

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
      testGroup "predicates" predicatesTests
    ]

predicatesTests :: [TestTree]
predicatesTests =
  [ testProperty "isSpace" $ predicatesProperty Char.isSpace,
    testProperty "isAlpha" $ predicatesProperty Char.isAlpha,
    testProperty "isPrint" $ predicatesProperty Char.isPrint,
    testProperty "all" $ predicatesProperty (const True),
    testProperty "isLetter" $ predicatesProperty Char.isLetter,
    testProperty "isLower" $ predicatesProperty Char.isLower,
    testProperty "isUpper" $ predicatesProperty Char.isUpper
  ]
