module Text.HLex.Internal.Nfa
  ( Nfa (..),
    State (..),
    newNfa,
    defState,
  )
where

import Data.HashSet (HashSet)
import Data.Vector qualified as VB
import Data.Word (Word8)
import Text.HLex.Internal.Range (Range)

data Nfa a = Nfa
  { starts :: [Int],
    states :: !(VB.Vector (State a))
  }

data State a = State
  { transitions :: [(Range Word8, Int)],
    emptyTransitions :: !(HashSet Int),
    accept :: Maybe a
  }

newNfa :: Nfa a
newNfa = Nfa {starts = mempty, states = mempty}

defState :: State a
defState = State {transitions = mempty, emptyTransitions = mempty, accept = Nothing}
