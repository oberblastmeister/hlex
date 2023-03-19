module Text.HLex.Internal.NfaToDfa
  ( nfaToDfa,
  )
where

import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.RangeSet.List qualified as RSet
import Data.Vector qualified as VB
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
    ndfa' = Dfa.addPdfa ns (Dfa.State {transitions, accept}) ndfa
    nss' = ((`Nfa.closure` nfa) <$> IntMap.elems transitions) ++ nss
    -- TODO: support right contexts
    accept =
      fmap NE.head
        . NE.nonEmpty
        $ List.sort
          [ acc
            | s <- IntSet.toList ns,
              acc <- nfa & Nfa.states & (VB.! s) & Nfa.accept & Foldable.toList
          ]
    transitions =
      IntMap.fromListWith
        (<>)
        [ (fromIntegral @_ @Int byte, IntSet.singleton to)
          | from <- IntSet.toList ns,
            (byteSet, to) <- nfa & Nfa.states & (VB.! from) & Nfa.transitions,
            byte <- RSet.toList byteSet
        ]
