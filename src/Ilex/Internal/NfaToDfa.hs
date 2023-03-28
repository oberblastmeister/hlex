module Ilex.Internal.NfaToDfa
  ( nfaToDfa,
  )
where

import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Ilex.Internal.Dfa (Dfa, Dfa' (Dfa), Pdfa)
import Ilex.Internal.Dfa qualified as Dfa
import Ilex.Internal.Nfa (Nfa (Nfa))
import Ilex.Internal.Nfa qualified as Nfa
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE

nfaToDfa :: Ord a => Nfa a -> Dfa (NonEmpty a)
nfaToDfa = Dfa.normalize . nfaToPdfa

nfaToPdfa :: Ord a => Nfa a -> Pdfa (NonEmpty a)
nfaToPdfa nfa@Nfa {start} = nfaToPdfa' nfa ndfa [start']
  where
    ndfa = Dfa {start = start', states = mempty}
    start' = Nfa.closure (IntSet.singleton start) nfa

nfaToPdfa' :: Ord a => Nfa a -> Pdfa (NonEmpty a) -> [Dfa.StateSet] -> Pdfa (NonEmpty a)
nfaToPdfa' _nfa pdfa [] = pdfa
nfaToPdfa' nfa ndfa (ns : nss)
  | ns `Dfa.inPdfa` ndfa = nfaToPdfa' nfa ndfa nss
  | otherwise = nfaToPdfa' nfa ndfa' nss'
  where
    ndfa' = Dfa.addPdfa ns Dfa.State {transitions, accept, isCharEnd} ndfa
    nss' = IntMap.elems transitions ++ nss
    -- TODO: support right contexts
    isCharEnd = Nfa.chooseIsCharEnd ns nfa
    accept = NE.nonEmpty $ Nfa.listAccepts ns nfa
    transitions = Nfa.transitionMapClosure ns nfa
