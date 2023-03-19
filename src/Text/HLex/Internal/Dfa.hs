{-# LANGUAGE UndecidableInstances #-}

module Text.HLex.Internal.Dfa
  ( Pdfa,
    Dfa,
    Dfa' (..),
    State (..),
    PState,
    StateSet,
    inPdfa,
    addPdfa,
    forFromTransTo,
    toAssocList,
    assocs,
    transform,
    normalize,
    newState,
    emptyState,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.IntMap (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.IntSet (IntSet)
import Data.Maybe (fromJust)
import Data.Vector.Persistent qualified as PVec
import GHC.Exts (fromList)
import Text.HLex.Internal.AssocList (AssocList)

-- dfa using sets of nfa states
type Pdfa = Dfa' (HashMap StateSet) StateSet

type Dfa = Dfa' PVec.Vector Int

data Dfa' f s a = Dfa
  { start :: s,
    states :: !(f (State s a))
  }

deriving instance (Show a, Show s) => Show (Dfa' (AssocList Int) s a)

instance Show a => Show (Dfa a) where
  show = show . toAssocList

-- deriving instance (Show s, Show (f (State s a))) => Show (Dfa' f s a)

instance Functor f => Bifunctor (Dfa' f) where
  bimap f g Dfa {start, states} =
    Dfa
      { start = f start,
        states = fmap (bimap f g) states
      }

data State s a = State
  { transitions :: !(IntMap s),
    accept :: Maybe a
  }
  deriving (Show)

type PState = State StateSet

type StateSet = IntSet

instance Bifunctor State where
  bimap f g State {transitions, accept} =
    State
      { transitions = fmap f transitions,
        accept = fmap g accept
      }

newState :: IntMap s -> State s a
newState transitions = State {transitions, accept = Nothing}

emptyState :: State s a
emptyState = State mempty Nothing

toAssocList :: Dfa a -> Dfa' (AssocList Int) Int a
toAssocList = transform $ fromList @(AssocList _ _) . zip [0 :: Int ..] . PVec.toList

assocs :: Dfa a -> [(Int, State Int a)]
assocs Dfa {states} = zip [0 :: Int ..] $ PVec.toList states

inPdfa :: StateSet -> Pdfa a -> Bool
inPdfa set Dfa {states} = set `HashMap.member` states

addPdfa :: StateSet -> PState a -> Pdfa a -> Pdfa a
addPdfa set s dfa@Dfa {states} = dfa {states = HashMap.insert set s states}

forFromTransTo :: Dfa a -> (Int -> Int -> Int -> [b]) -> [b]
forFromTransTo Dfa {states} f = do
  (from, state) <- zip [0 :: Int ..] $ PVec.toList states
  (trans, to) <- IntMap.toList $ transitions state
  f from trans to
{-# INLINE forFromTransTo #-}

transform :: (forall a. f a -> g a) -> Dfa' f s a -> Dfa' g s a
transform f dfa@Dfa {states} = dfa {states = f states}

normalize :: forall s a. Hashable s => Dfa' (HashMap s) s a -> Dfa a
normalize Dfa {states, start} = Dfa {states = states', start = start'}
  where
    states' =
      PVec.fromList
        [ convertState s
          | (ns, _) <- stateList,
            let s = fromJust $ HashMap.lookup ns states
        ]

    start' = getState start

    convertState :: State s a -> State Int a
    convertState state@State {transitions} = state {transitions = getState <$> transitions}

    getState :: s -> Int
    getState = fromJust . flip HashMap.lookup stateMap

    stateList = zip (HashMap.keys states) [0 :: Int ..]

    stateMap = fromList stateList
