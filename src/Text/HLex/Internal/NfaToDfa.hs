module Text.HLex.Internal.NfaToDfa
  ( nfaToDfa,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Text.HLex.Internal.Dfa (Dfa, Dfa' (Dfa), Pdfa)
import Text.HLex.Internal.Dfa qualified as Dfa
import Text.HLex.Internal.Nfa (Nfa (Nfa))
import Text.HLex.Internal.Nfa qualified as Nfa

nfaToDfa :: Ord a => Nfa a -> Dfa a
nfaToDfa = Dfa.normalize . nfaToPdfa

nfaToPdfa :: Ord a => Nfa a -> Pdfa a
nfaToPdfa nfa@Nfa {start} = nfaToPdfa' nfa ndfa [start']
  where
    ndfa = Dfa {start = start', states = mempty}
    start' = Nfa.closure (IntSet.singleton start) nfa

nfaToPdfa' :: Ord a => Nfa a -> Pdfa a -> [Dfa.StateSet] -> Pdfa a
nfaToPdfa' _nfa pdfa [] = pdfa
nfaToPdfa' nfa ndfa (ns : nss)
  | ns `Dfa.inPdfa` ndfa = nfaToPdfa' nfa ndfa nss
  | otherwise = nfaToPdfa' nfa ndfa' nss'
  where
    ndfa' = Dfa.addPdfa ns Dfa.State {transitions, accept} ndfa
    nss' = IntMap.elems transitions ++ nss
    -- TODO: support right contexts
    accept = Nfa.chooseAccept ns nfa
    transitions = Nfa.transitionMapClosure ns nfa
