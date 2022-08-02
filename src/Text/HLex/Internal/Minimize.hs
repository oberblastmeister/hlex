module Text.HLex.Internal.Minimize where

import Data.Bifunctor (first)
import Data.Either qualified as Either
import Data.Foldable (foldMap', foldl')
import Data.Function (on, (&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Hashable (Hashable)
import Data.List qualified as List
import Data.Maybe (fromJust)
import Debug.Trace
import GHC.Exts (fromList, toList)
import Text.HLex.Internal.Accept qualified as Accept
import Text.HLex.Internal.AssocList qualified as AssocList
import Text.HLex.Internal.Dfa (Dfa, NDfa)
import Text.HLex.Internal.Dfa qualified as Dfa

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
minimize :: Dfa a -> Dfa a
minimize dfa = Dfa.normalize dfa'
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
    q = fromList @(HashSet _) $ acceptingSets

    !_ = traceId $ "acceptingSets: " ++ show acceptingSets
    !_ = traceId $ "nonAcceptingSet: " ++ show nonAcceptingSet

    -- !_ = traceId $ "bigMap: " ++ show bigMap

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

    -- trans -> to -> from
    bigMap :: HashMap Int (HashMap Int Dfa.StateSet)
    bigMap = HashMap.fromListWith (HashMap.unionWith HashSet.union) $
      Dfa.forFromTransTo dfa $ \from trans to ->
        [(trans, HashMap.singleton to (HashSet.singleton from))]

    preimage :: HashMap Int Dfa.StateSet -> Dfa.StateSet -> Dfa.StateSet
    preimage invMap a = foldMap' id $ HashMap.intersection invMap $ HashSet.toMap a

    go :: [Dfa.StateSet] -> HashSet (Dfa.StateSet) -> [Dfa.StateSet]
    go p q | trace ("p: " ++ show p ++ " q: " ++ show q) False = undefined
    go !p (takeSet -> Just (a, q)) = go p' q'
      where
        !_ = traceId $ "a: " ++ show a
        (p', q') = foldl' go0 (p, q) xs
        !_ = traceId $ "xs: " ++ show xs
        xs =
          [ x
            | invMap <- HashMap.elems bigMap,
              let x = preimage invMap a,
              not $ HashSet.null x
          ]
        go0 (!p, !q) x = foldl' go1 ([], q) p
          where
            go1 (pAcc, q) y
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
              | otherwise = (y : pAcc, q)
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
