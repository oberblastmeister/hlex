{-# LANGUAGE UndecidableInstances #-}

module Text.HLex.Internal.Dfa
  ( Pdfa,
    Dfa,
    Dfa' (..),
    State' (..),
    State,
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
    simulate,
    valid,
  )
where

import Control.Applicative ((<|>))
import Data.Bifunctor (Bifunctor (bimap))
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.IntMap (IntMap)
import Data.IntMap qualified as IntMap
import Data.IntSet (IntSet)
import Data.Maybe (fromJust)
import Data.Vector qualified as VB
import Data.Word (Word8)
import GHC.Exts (fromList)
import Text.HLex.Internal.AssocList (AssocList)

-- dfa using sets of nfa states
type Pdfa = Dfa' (HashMap StateSet) StateSet

type Dfa = Dfa' VB.Vector Int

type State = State' Int

data Dfa' f s a = Dfa
  { start :: s,
    states :: !(f (State' s a))
  }

deriving instance (Show a, Show s) => Show (Dfa' (AssocList Int) s a)

instance Show a => Show (Dfa a) where
  show = show . toAssocList

instance Functor f => Bifunctor (Dfa' f) where
  bimap f g Dfa {start, states} =
    Dfa
      { start = f start,
        states = fmap (bimap f g) states
      }

data State' s a = State
  { transitions :: !(IntMap s),
    isCharEnd :: !Bool,
    accept :: Maybe a
  }
  deriving (Show, Eq)

type PState = State' StateSet

type StateSet = IntSet

instance Bifunctor State' where
  bimap f g state@State {transitions, accept} =
    state
      { transitions = fmap f transitions,
        accept = fmap g accept
      }

valid :: Dfa a -> Bool
valid Dfa {start, states} = validStateId start && all validState states
  where
    validState State {transitions} =
      all validTransition (IntMap.toList transitions)
    validTransition (_, to) = validStateId to
    validStateId s = s >= 0 && s < VB.length states

newState :: IntMap s -> State' s a
newState transitions = State {transitions, accept = Nothing, isCharEnd = False}

emptyState :: State' s a
emptyState = State mempty False Nothing

toAssocList :: Dfa a -> Dfa' (AssocList Int) Int a
toAssocList = transform $ fromList @(AssocList _ _) . zip [0 :: Int ..] . VB.toList

assocs :: Dfa a -> [(Int, State a)]
assocs Dfa {states} = zip [0 :: Int ..] $ VB.toList states

inPdfa :: StateSet -> Pdfa a -> Bool
inPdfa set Dfa {states} = set `HashMap.member` states

addPdfa :: StateSet -> PState a -> Pdfa a -> Pdfa a
addPdfa set s dfa@Dfa {states} = dfa {states = HashMap.insert set s states}

forFromTransTo :: Dfa a -> (Int -> Int -> Int -> [b]) -> [b]
forFromTransTo Dfa {states} f = do
  (from, state) <- zip [0 :: Int ..] $ VB.toList states
  (trans, to) <- IntMap.toList $ transitions state
  f from trans to
{-# INLINE forFromTransTo #-}

transform :: (forall a. f a -> g a) -> Dfa' f s a -> Dfa' g s a
transform f dfa@Dfa {states} = dfa {states = f states}

normalize :: forall s a. Hashable s => Dfa' (HashMap s) s a -> Dfa a
normalize Dfa {states, start} = Dfa {states = states', start = start'}
  where
    states' =
      VB.fromList
        [ convertState s
          | (ns, _) <- stateList,
            let s = fromJust $ HashMap.lookup ns states
        ]

    start' = getState start

    convertState :: State' s a -> State a
    convertState state@State {transitions} = state {transitions = getState <$> transitions}

    getState :: s -> Int
    getState = fromJust . flip HashMap.lookup stateMap

    stateList = zip (HashMap.keys states) [0 :: Int ..]

    stateMap = fromList stateList

simulate :: ByteString -> Dfa a -> Maybe (Int, Int, a)
simulate bs Dfa {start, states} = go start (0 :: Int) (0 :: Int) Nothing
  where
    go !s !index !charIndex !lastMatch = case B.indexMaybe bs index of
      Nothing -> result
      Just b ->
        case IntMap.lookup (fromIntegral @Word8 @Int b) (transitions state) of
          Nothing -> result
          Just s' -> go s' (index + 1) newCharIndex newMatch
      where
        state = states VB.! s
        newCharIndex = charIndex + if isCharEnd state then 1 else 0
        newMatch = (index,newCharIndex,) <$> accept state
        result = newMatch <|> lastMatch
