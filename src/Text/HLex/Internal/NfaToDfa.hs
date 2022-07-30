module Text.HLex.Internal.NfaToDfa
  ( minimizeDfa,
    nfaToDfa,
  )
where

import Data.Bifunctor (first, second)
import Data.Either qualified as Either
import Data.Foldable (foldMap', foldl')
import Data.Foldable qualified as Foldable
import Data.Function (on, (&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromJust)
import Data.Vector qualified as VB
import Data.Vector.Persistent qualified as PVec
import GHC.Exts (fromList, toList)
import Text.HLex.Internal.Accept qualified as Accept
import Text.HLex.Internal.AssocList qualified as AssocList
import Text.HLex.Internal.Dfa (Dfa, Dfa' (Dfa), NDfa)
import Text.HLex.Internal.Dfa qualified as Dfa
import Text.HLex.Internal.Nfa (Nfa (Nfa))
import Text.HLex.Internal.Nfa qualified as Nfa
import Text.HLex.Internal.RangeMap (RangeMap)
import Text.HLex.Internal.RangeMap qualified as RangeMap

nfaToDfa :: Nfa a -> Dfa a
nfaToDfa = normalizeDfa . nfaToNDfa

normalizeDfa :: forall s a. Hashable s => Dfa' (HashMap s) s a -> Dfa a
normalizeDfa Dfa {states, starts} = Dfa {states = states', starts = starts'}
  where
    states' =
      PVec.fromList
        [ convertState s
          | (ns, _) <- stateList,
            let s = fromJust $ HashMap.lookup ns states
        ]

    starts' = getState <$> starts

    convertState :: Dfa.State s a -> Dfa.State Int a
    convertState state@Dfa.State {transitions} = state {Dfa.transitions = getState <$> transitions}

    getState :: s -> Int
    getState = fromJust . flip HashMap.lookup stateMap

    stateList = zip (HashMap.keys states) [0 :: Int ..]

    stateMap :: HashMap s Int
    stateMap = fromList stateList

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
    accept =
      fmap NE.head $
        NE.nonEmpty $
          List.sortOn
            Accept.priority
            [ acc
              | s <- toList ns,
                acc <- nfa & Nfa.states & (VB.! s) & Nfa.accept & Foldable.toList
            ]
    byteTrans =
      fmap (second (`Nfa.closure` nfa)) $
        RangeMap.elems $
          fromList @(RangeMap _) $ fmap (second HashSet.singleton) rangeTrans
    rangeTrans = [t | s <- toList ns, t <- nfa & Nfa.states & (VB.! s) & Nfa.transitions]

-- % Hopcroft's Algorithm for DFA minimization (cut/pasted from Wikipedia):
-- % X refines Y into Y1 and Y2 means
-- %  Y1 := Y ∩ X
-- %  Y2 := Y \ X
-- %  where both Y1 and Y2 are nonempty
--
-- P := {{all accepting states}, {all nonaccepting states}};
-- Q := {{all accepting states}};
-- while (Q is not empty) do
--      choose and remove a set A from Q
--      for each c in ∑ do
--           let X be the set of states for which a transition on c leads to a state in A
--           for each set Y in P for which X refines Y into Y1 and Y2 do
--                replace Y in P by the two sets Y1 and Y2
--                if Y is in Q
--                     replace Y in Q by the same two sets
--                else
--                     add the smaller of the two sets to Q
--           end;
--      end;
-- end;
minimizeDfa :: Dfa a -> Dfa a
minimizeDfa dfa = normalizeDfa dfa'
  where
    dfa' =
      Dfa.toAssocList dfa
        & first getState
        & Dfa.transform (AssocList.toHashMap . first getState)

    getState :: Int -> Int
    getState = fromJust . flip HashMap.lookup equivMap

    equivMap :: HashMap Int Int
    equivMap =
      fromList
        [ (s, rep)
          | states <- equivalent,
            let rep = headSet states,
            s <- toList states
        ]

    equivalent = dfaEquivalentStates dfa

dfaEquivalentStates :: Dfa a -> [Dfa.StateSet]
dfaEquivalentStates dfa = go p q
  where
    p = acceptingSets ++ [nonAcceptingSet]
    q = fromList @(HashSet _) acceptingSets

    -- we need to distinguish accepting states from each other
    -- unless they have the same priority
    acceptingSets =
      acceptingAssoc
        & List.sortOn (Accept.priority . snd)
        & List.groupBy ((==) `on` Accept.priority . snd)
        & (fmap . fmap) fst
        & fmap (fromList @(HashSet _))

    ( acceptingAssoc,
      fromList @(HashSet _) -> nonAcceptingSet
      ) =
        dfa & Dfa.assocs
          & fmap
            ( \(i, Dfa.State {accept}) -> case accept of
                Just accept -> Left (i, accept)
                Nothing -> Right i
            )
          & Either.partitionEithers

    bigMap :: HashMap Int (HashMap Int Dfa.StateSet)
    bigMap = HashMap.fromListWith (HashMap.unionWith HashSet.union) $
      Dfa.forFromTransTo dfa $ \from trans to ->
        [(trans, HashMap.singleton to (HashSet.singleton from))]

    preimage :: HashMap Int Dfa.StateSet -> Dfa.StateSet -> Dfa.StateSet
    preimage invMap a = foldMap' id $ HashMap.intersection invMap $ HashSet.toMap a

    go :: [Dfa.StateSet] -> HashSet (Dfa.StateSet) -> [Dfa.StateSet]
    go !p (takeSet -> Just (a, q)) =
      if HashSet.null q
        then p
        else go p' q'
      where
        (p', q') = foldl' go0 (p, q) xs
        xs =
          [ x
            | invMap <- HashMap.elems bigMap,
              let x = preimage invMap a,
              not $ HashSet.null x
          ]
        go0 (!p, !q) x = foldl' go1 ([], q) p
          where
            go1 s@(pAcc, q) y
              | (y1, y2) <- refineSet x y,
                not $ HashSet.null y1,
                not $ HashSet.null y2 = do
                  let !q' =
                        if HashSet.member y q
                          then HashSet.insert y2 $ HashSet.insert y1 q
                          else
                            HashSet.insert
                              ( if HashSet.size y1 < HashSet.size y2
                                  then y1
                                  else y2
                              )
                              q
                  (y1 : y2 : pAcc, q')
              | otherwise = s
    go p _ = p

refineSet :: Hashable k => HashSet k -> HashSet k -> (HashSet k, HashSet k)
refineSet setX setY = (setY1, setY2)
  where
    setY1 = setY `HashSet.intersection` setX
    setY2 = setY `HashSet.difference` setX

headSet :: Hashable a => HashSet a -> a
headSet = head . toList

takeSet :: Hashable a => HashSet a -> Maybe (a, HashSet a)
takeSet set = case toList set of
  [] -> Nothing
  x : _ -> Just (x, HashSet.delete x set)
