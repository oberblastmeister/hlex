module Regex where

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Text.HLex.Internal.CharSet qualified as CharSet
import Text.HLex.Internal.Regex

genWith :: Gen Char -> Gen Regex
genWith genChar = go
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
