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
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.HashSet (HashSet)
import Data.Vector.Persistent.Strict qualified as PVec
import GHC.Exts (fromList, toList)
import Text.HLex.Internal.Accept (Accept)
import Text.HLex.Internal.AssocList (AssocList)

type NDfa = Dfa' (HashMap StateSet) StateSet

type Dfa = Dfa' PVec.Vector Int

data Dfa' f s a = Dfa
  { starts :: [s],
    states :: !(f (State s a))
  }

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

type NState = State StateSet

type StateSet = HashSet Int

instance Bifunctor State where
  bimap f g State {transitions, accept} =
    State
      { transitions = fmap f transitions,
        accept = (fmap . fmap) g accept
      }

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
