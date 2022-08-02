{-# LANGUAGE UndecidableInstances #-}

module Text.HLex.Internal.Dfa
  ( NDfa,
    Dfa,
    Dfa' (..),
    State (..),
    NState,
    StateSet,
    inNDfa,
    addNDfa,
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
import Data.HashSet (HashSet)
import Data.Hashable (Hashable)
import Data.Maybe (fromJust)
import Data.Vector.Persistent qualified as PVec
import GHC.Exts (fromList, toList)
import Text.HLex.Internal.Accept (Accept)
import Text.HLex.Internal.AssocList (AssocList)

-- dfa using sets of nfa states
type NDfa = Dfa' (HashMap StateSet) StateSet

type Dfa = Dfa' PVec.Vector Int

data Dfa' f s a = Dfa
  { starts :: [s],
    states :: !(f (State s a))
  }

deriving instance (Show a, Show s) => Show (Dfa' (AssocList Int) s a)

instance Show a => Show (Dfa a) where
  show = show . toAssocList

-- deriving instance (Show s, Show (f (State s a))) => Show (Dfa' f s a)

instance Functor f => Bifunctor (Dfa' f) where
  bimap f g Dfa {starts, states} =
    Dfa
      { starts = fmap f starts,
        states = fmap (bimap f g) states
      }

data State s a = State
  { transitions :: !(HashMap Int s),
    accept :: Maybe (Accept a)
  }
  deriving (Show)

type NState = State StateSet

type StateSet = HashSet Int

instance Bifunctor State where
  bimap f g State {transitions, accept} =
    State
      { transitions = fmap f transitions,
        accept = (fmap . fmap) g accept
      }

newState :: HashMap Int s -> State s a
newState transitions = State {transitions, accept = Nothing}

emptyState :: State s a
emptyState = State mempty Nothing

toAssocList :: Dfa a -> Dfa' (AssocList Int) Int a
toAssocList = transform $ fromList @(AssocList _ _) . zip [0 :: Int ..] . PVec.toList

assocs :: Dfa a -> [(Int, State Int a)]
assocs Dfa {states} = zip [0 :: Int ..] $ toList states

inNDfa :: StateSet -> NDfa a -> Bool
inNDfa set Dfa {states} = set `HashMap.member` states

addNDfa :: StateSet -> NState a -> NDfa a -> NDfa a
addNDfa set s dfa@Dfa {states} = dfa {states = HashMap.insert set s states}

forFromTransTo :: Dfa a -> (Int -> Int -> Int -> [b]) -> [b]
forFromTransTo Dfa {states} f = do
  (from, state) <- zip [0 :: Int ..] $ PVec.toList states
  (trans, to) <- HashMap.toList $ transitions state
  f from trans to
{-# INLINE forFromTransTo #-}

transform :: (forall a. f a -> g a) -> Dfa' f s a -> Dfa' g s a
transform f dfa@Dfa {states} = dfa {states = f states}

normalize :: forall s a. Hashable s => Dfa' (HashMap s) s a -> Dfa a
normalize Dfa {states, starts} = Dfa {states = states', starts = starts'}
  where
    states' =
      PVec.fromList
        [ convertState s
          | (ns, _) <- stateList,
            let s = fromJust $ HashMap.lookup ns states
        ]

    starts' = getState <$> starts

    convertState :: State s a -> State Int a
    convertState state@State {transitions} = state {transitions = getState <$> transitions}

    getState :: s -> Int
    getState = fromJust . flip HashMap.lookup stateMap

    stateList = zip (HashMap.keys states) [0 :: Int ..]

    stateMap :: HashMap s Int
    stateMap = fromList stateList
