module Text.HLex.Internal.Dfa.Simulate where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.IntMap qualified as IntMap
import Data.Vector.Persistent qualified as PVec
import Data.Word (Word8)
import Text.HLex.Internal.Dfa (Dfa, Dfa' (Dfa))
import Text.HLex.Internal.Dfa qualified as Dfa

simulate :: ByteString -> Dfa a -> Maybe (Int, a)
simulate bs Dfa {Dfa.start, Dfa.states} = go start 0 Nothing
  where
    go s i lastMatch = case B.indexMaybe bs i of
      Nothing -> (i,) <$> Dfa.accept (PVec.index s states)
      Just b -> do
        let state = PVec.index s states
        case IntMap.lookup (fromIntegral @Word8 @Int b) (Dfa.transitions state) of
          Nothing -> (i,) <$> lastMatch
          Just s' -> go s' (i + 1) (Dfa.accept state <|> lastMatch)
