module Text.HLex.Internal.Nfa
  ( Nfa (..),
    State (..),
    ByteSet,
    StateId,
    StateSet,
    newNfa,
    defState,
    closure,
  )
where

import Data.Foldable (foldl')
import Data.Function ((&))
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.Vector qualified as VB
import GHC.Exts (toList)
import Text.HLex.Internal.Utils

type StateId = Int

type StateSet = IntSet

data Nfa a = Nfa
  { starts :: [StateId],
    states :: !(VB.Vector (State a))
  }

data State a = State
  { transitions :: [(ByteSet, StateId)],
    emptyTransitions :: !IntSet,
    accept :: Maybe a
  }

newNfa :: Nfa a
newNfa = Nfa {starts = mempty, states = mempty}

defState :: State a
defState = State {transitions = mempty, emptyTransitions = mempty, accept = Nothing}

closure :: StateSet -> Nfa a -> StateSet
closure starts nfa = go starts $ toList starts
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
