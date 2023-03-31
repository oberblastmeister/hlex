module Hlex.Internal.Minimize
  ( minimize,
    dfaEquivalentStates,
    validEquivs,
    isMinimal,
  )
where

import Data.Bifunctor (first)
import Data.Either qualified as Either
import Data.Foldable (foldMap', foldl')
import Data.Function (on, (&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Vector qualified as VB
import GHC.Exts (fromList, toList)
import Hlex.Internal.AssocList qualified as AssocList
import Hlex.Internal.Dfa (Dfa, Dfa' (Dfa))
import Hlex.Internal.Dfa qualified as Dfa

-- % Hopcroft's Algorithm for DFA minimization (cut/pasted from Wikipedia): % X refines Y into Y1 and Y2 means
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
minimize :: Ord a => Dfa a -> Dfa a
minimize = Dfa.normalize . minimize'

minimize' :: Ord a => Dfa a -> Dfa.Dfa' (HashMap Int) Int a
minimize' dfa = dfa'
  where
    dfa' =
      Dfa.toAssocList dfa
        & first getState
        & Dfa.transform (AssocList.toHashMap . first getState)

    getState :: Int -> Int
    getState = fromJust . flip HashMap.lookup equivMap

    equivMap :: HashMap Int Int
    equivMap =
      HashMap.fromList
        [ (s, rep)
          | states <- equivalent,
            let rep = headSet states,
            s <- toList states
        ]

    equivalent = dfaEquivalentStates dfa

dfaEquivalentStates :: Ord a => Dfa a -> [Dfa.StateSet]
dfaEquivalentStates dfa = go p q
  where
    p = acceptingSets ++ [nonAcceptingSet]
    q = fromList @(HashSet _) $ acceptingSets ++ [nonAcceptingSet]

    -- !_ = traceId $ "acceptingSets: " ++ show acceptingSets
    -- !_ = traceId $ "nonAcceptingSet: " ++ show nonAcceptingSet

    -- !_ = traceId $ "bigMap: " ++ show bigMap

    -- accepting states that have the same value will be in the same initial partition
    acceptingSets =
      fmap (IntSet.fromList . fmap fst) $
        List.groupBy ((==) `on` snd) $
          List.sortOn snd acceptingStates

    nonAcceptingSet = IntSet.fromList nonAcceptingStates

    ( acceptingStates,
      nonAcceptingStates
      ) =
        dfa
          & Dfa.assocs
          & fmap
            ( \(i, Dfa.State {accept}) -> case accept of
                Just accept -> Left (i, accept)
                Nothing -> Right i
            )
          & Either.partitionEithers

    -- trans -> to -> from
    bigMap :: IntMap (IntMap Dfa.StateSet)
    bigMap = IntMap.fromListWith (IntMap.unionWith IntSet.union) $
      Dfa.forFromTransTo dfa $ \from trans to ->
        [(trans, IntMap.singleton to (IntSet.singleton from))]

    preimage :: IntMap Dfa.StateSet -> Dfa.StateSet -> Dfa.StateSet
    preimage invMap a = foldMap' id $ IntMap.intersection invMap $ IntMap.fromSet (const ()) a

    go :: [Dfa.StateSet] -> HashSet Dfa.StateSet -> [Dfa.StateSet]
    -- go p q | trace ("p: " ++ show p ++ " q: " ++ show q) False = undefined
    go !p (takeSet -> Just (a, q)) = go p' q'
      where
        -- !_ = traceId $ "a: " ++ show a
        (p', q') = foldl' go0 (p, q) xs
        -- !_ = traceId $ "xs: " ++ show xs
        xs =
          [ x
            | invMap <- IntMap.elems bigMap,
              let x = preimage invMap a,
              not $ IntSet.null x
          ]
        go0 (!p, !q) x = foldl' go1 ([], q) p
          where
            go1 (pAcc, q) y
              | (y1, y2) <- refineSet x y,
                not $ IntSet.null y1,
                not $ IntSet.null y2 = do
                  let !q' =
                        if HashSet.member y q
                          then HashSet.insert y2 $ HashSet.insert y1 q
                          else
                            HashSet.insert
                              ( if IntSet.size y1 < IntSet.size y2
                                  then y1
                                  else y2
                              )
                              q
                  (y1 : y2 : pAcc, q')
              | otherwise = (y : pAcc, q)
    go p _ = p

refineSet :: IntSet -> IntSet -> (IntSet, IntSet)
refineSet setX setY = (setY1, setY2)
  where
    setY1 = setY `IntSet.intersection` setX
    setY2 = setY `IntSet.difference` setX

headSet :: IntSet -> Int
headSet = head . toList

takeSet :: Hashable a => HashSet a -> Maybe (a, HashSet a)
takeSet set = case toList set of
  [] -> Nothing
  x : _ -> Just (x, HashSet.delete x set)

validEquivs :: [Dfa.StateSet] -> Bool
validEquivs equivs = equivs == List.nub equivs

-- might not be correct
isMinimal :: [Dfa.StateSet] -> Dfa a -> Bool
isMinimal equivStates Dfa {states} =
  correctSize && validEquivs equivStates && flip all equivStates \equiv -> do
    let allTransitions =
          foldMap'
            id
            [ pure @[] . fromJust . (`HashMap.lookup` equivMap) <$> transitions
              | s <- IntSet.toList equiv,
                let state = states VB.! s,
                let transitions = Dfa.transitions state
            ]
    flip all allTransitions \equivs -> length (List.nub equivs) == 1
  where
    correctSize = IntSet.size (IntSet.unions equivStates) == VB.length states
    equivMap =
      HashMap.fromList
        [ (s, equiv)
          | equiv <- equivStates,
            s <- IntSet.toList equiv
        ]
