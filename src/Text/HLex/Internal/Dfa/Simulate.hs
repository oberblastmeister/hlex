module Text.HLex.Internal.Dfa.Simulate where

import Data.ByteString (ByteString)
import Text.HLex.Internal.Dfa (Dfa)

simulate :: ByteString -> Dfa a -> Maybe a
simulate = undefined
