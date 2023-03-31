module Hlex.Internal.Nfa
  ( Nfa (..),
    State (..),
    ByteSet,
    StateId,
    StateSet,
    defState,
    closure,
    individualTransitions,
    transitionMap,
    chooseAccept,
    simulate,
    transitionMapClosure,
    valid,
    chooseIsCharEnd,
    listAccepts,
  )
where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.Foldable (foldl')
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as VB
import Data.Word (Word8)
import GHC.Exts (toList)
import Hlex.Internal.Utf8 (Utf8Range)
import Hlex.Internal.Utils
import Numeric.Interval.NonEmpty qualified as I

type StateId = Int

type StateSet = IntSet

data Nfa a = Nfa
  { start :: !StateId,
    states :: !(VB.Vector (State a))
  }
  deriving (Show, Eq)

data State a = State
  { transitions :: [(Utf8Range, StateId)],
    emptyTransitions :: !IntSet,
    accept :: Maybe a,
    isCharEnd :: !Bool
  }
  deriving (Show, Eq)

defState :: State a
defState = State {transitions = mempty, emptyTransitions = mempty, accept = Nothing, isCharEnd = False}

valid :: Nfa a -> Bool
valid Nfa {start, states} = validStateId start && all validState (VB.toList states)
  where
    validState State {transitions, emptyTransitions} =
      all validTransition transitions
        && all validStateId (IntSet.toList emptyTransitions)
    validTransition (_, to) = validStateId to
    validStateId s = s >= 0 && s < VB.length states

listAccepts :: StateSet -> Nfa a -> [a]
listAccepts ss nfa =
  [ acc
    | s <- IntSet.toList ss,
      acc <- nfa & states & (VB.! s) & accept & Foldable.toList
  ]

chooseAccept :: (Ord a) => StateSet -> Nfa a -> Maybe a
chooseAccept ss nfa =
  fmap NE.head
    . NE.nonEmpty
    $ List.sort
      [ acc
        | s <- IntSet.toList ss,
          acc <- nfa & states & (VB.! s) & accept & Foldable.toList
      ]

chooseIsCharEnd :: StateSet -> Nfa a -> Bool
chooseIsCharEnd ss nfa =
  -- we use any instead of all here because the closure of a state set might not
  -- all have the same isCharEnd status
  any
    (\s -> nfa & states & (VB.! s) & isCharEnd)
    (IntSet.toList ss)

transitionMapClosure :: StateSet -> Nfa a -> IntMap StateSet
transitionMapClosure ss nfa = (`closure` nfa) <$> transitionMap ss nfa

transitionMap :: StateSet -> Nfa a -> IntMap StateSet
transitionMap ss nfa =
  IntMap.fromListWith
    (<>)
    [ (byte, IntSet.singleton to)
      | from <- IntSet.toList ss,
        (byte, to) <- individualTransitions from nfa
    ]

individualTransitions :: StateId -> Nfa a -> [(Int, StateId)]
individualTransitions from nfa =
  [ (fromIntegral @_ @Int byte, to)
    | (utf8Range, to) <- nfa & states & (VB.! from) & transitions,
      byte <- [I.inf utf8Range .. I.sup utf8Range]
  ]

closure :: StateSet -> Nfa a -> StateSet
closure start nfa = go start $ toList start
  where
    go set [] = set
    go set (s : ss) = go set' ss'
      where
        etrans = nfa & states & (VB.! s) & emptyTransitions & toList
        (!set', !ss') =
          foldl'
            ( \(!set, !ss) s ->
                if IntSet.member s set
                  then (set, ss)
                  else (IntSet.insert s set, s : ss)
            )
            (set, ss)
            etrans

simulate :: (Ord a) => ByteString -> Nfa a -> Maybe (Int, Int, a)
simulate bs nfa@Nfa {start, states} = go (closure (IntSet.singleton start) nfa) (0 :: Int) (0 :: Int) Nothing
  where
    go !stateSet !index !charIndex !lastMatch = case B.indexMaybe bs index of
      Nothing -> result
      Just b -> case IntMap.lookup (fromIntegral @Word8 @Int b) map of
        Just ss -> go ss (index + 1) newCharIndex newMatch
        Nothing -> result
      where
        map = transitionMapClosure stateSet Nfa {start, states}
        accept = chooseAccept stateSet nfa
        isCharEnd = chooseIsCharEnd stateSet nfa
        newCharIndex = charIndex + if isCharEnd then 1 else 0
        newMatch = (index,newCharIndex,) <$> accept
        result = newMatch <|> lastMatch
