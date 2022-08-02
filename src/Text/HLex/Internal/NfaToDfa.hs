module Text.HLex.Internal.NfaToDfa
  ( nfaToDfa,
  )
where

import Data.Bifunctor (second)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.HashSet qualified as HashSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as VB
import GHC.Exts (fromList, toList)
import Text.HLex.Internal.Accept qualified as Accept
import Text.HLex.Internal.Dfa (Dfa, Dfa' (Dfa), NDfa)
import Text.HLex.Internal.Dfa qualified as Dfa
import Text.HLex.Internal.Nfa (Nfa (Nfa))
import Text.HLex.Internal.Nfa qualified as Nfa
import Text.HLex.Internal.RangeMap (RangeMap)
import Text.HLex.Internal.RangeMap qualified as RangeMap

nfaToDfa :: Nfa a -> Dfa a
nfaToDfa = Dfa.normalize . nfaToNDfa

nfaToNDfa :: Nfa a -> NDfa a
nfaToNDfa nfa@Nfa {starts} = nfaToNDfa' nfa ndfa init
  where
    ndfa = Dfa {starts = init, states = mempty}
    init = [Nfa.closure (fromList starts) nfa]

nfaToNDfa' :: Nfa a -> NDfa a -> [Dfa.StateSet] -> NDfa a
nfaToNDfa' _nfa pdfa [] = pdfa
nfaToNDfa' nfa ndfa (ns : nss)
  | ns `Dfa.inNDfa` ndfa = nfaToNDfa' nfa ndfa nss
  | otherwise = nfaToNDfa' nfa ndfa' nss'
  where
    ndfa' = Dfa.addNDfa ns (Dfa.State {transitions = fromList byteTrans, accept}) ndfa
    nss' = fmap snd byteTrans ++ nss
    -- choose the accept with the highest priority from the set of nfa states
    -- TODO: support right contexts
    accept =
      fmap NE.head
        . NE.nonEmpty
        $ List.sortOn
          Accept.priority
          [ acc
            | s <- toList ns,
              acc <- nfa & Nfa.states & (VB.! s) & Nfa.accept & Foldable.toList
          ]
    byteTrans =
      fmap (second (`Nfa.closure` nfa))
        . RangeMap.elems
        . fromList @(RangeMap _)
        $ fmap (second HashSet.singleton) rangeTrans
    rangeTrans = [t | s <- toList ns, t <- nfa & Nfa.states & (VB.! s) & Nfa.transitions]
