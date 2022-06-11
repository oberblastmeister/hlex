module Text.HLex.Internal.Nfa
  ( Nfa (..),
    State (..),
    newNfa,
    defState,
    closure,
  )
where

import Data.Foldable (foldl')
import Data.Function ((&))
import Data.HashSet (HashSet)
import Data.HashSet qualified as HashSet
import Data.Vector qualified as VB
import Data.Word (Word8)
import GHC.Exts (toList)
import Text.HLex.Internal.Accept (Accept)
import Text.HLex.Internal.Range (Range)

data Nfa a = Nfa
  { starts :: [Int],
    states :: !(VB.Vector (State a))
  }

data State a = State
  { transitions :: [(Range Word8, Int)],
    emptyTransitions :: !(HashSet Int),
    accept :: Maybe (Accept a)
  }

newNfa :: Nfa a
newNfa = Nfa {starts = mempty, states = mempty}

defState :: State a
defState = State {transitions = mempty, emptyTransitions = mempty, accept = Nothing}

closure :: HashSet Int -> Nfa a -> HashSet Int
closure starts nfa = go starts $ toList starts
  where
    go set [] = set
    go set (s : ss) = go set' ss'
      where
        etrans = nfa & states & (VB.! s) & emptyTransitions & toList
        (!set', !ss') =
          foldl'
            ( \(!set, !ss) s ->
                if HashSet.member s set
                  then (set, ss)
                  else (HashSet.insert s set, s : ss)
            )
            (set, ss)
            etrans
