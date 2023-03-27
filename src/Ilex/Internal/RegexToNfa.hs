module Ilex.Internal.RegexToNfa
  ( lexerToNfa,
    regexToNfa,
  )
where

import Control.Monad.State.Strict (MonadState)
import Control.Monad.State.Strict qualified as State
import Data.Char qualified as Char
import Data.Foldable (foldlM, for_)
import Data.Foldable qualified as Foldable
import Data.IntSet qualified as IntSet
import Data.List.NonEmpty qualified as NE
import Data.Vector qualified as VB
import Data.Vector.Persistent qualified as PVec
import Ilex.Internal.CharSet (CharSet)
import Ilex.Internal.CharSet qualified as CharSet
import Ilex.Internal.Nfa (Nfa (Nfa))
import Ilex.Internal.Nfa qualified as Nfa
import Ilex.Internal.Regex qualified as RE
import Ilex.Internal.Rule (Rule (..))
import Ilex.Internal.Rule qualified as Rule
import Ilex.Internal.Utf8
import Ilex.Internal.Utils
import Numeric.Interval.NonEmpty ((...))

data NfaBuilder a = NfaBuilder
  { nfa :: !(PVec.Vector (Nfa.State a)),
    next :: !Nfa.StateId
  }

type MonadNfa a = MonadState (NfaBuilder a)

lexerToNfa :: [Rule a] -> Nfa a
lexerToNfa lexer =
  Nfa
    { Nfa.start,
      Nfa.states = VB.fromListN (PVec.length nfa) (PVec.toList nfa)
    }
  where
    (start, NfaBuilder {nfa}) = State.runState (lexerToNfa' lexer) newNfaBuilder

lexerToNfa' :: MonadState (NfaBuilder a) m => [Rule a] -> m Int
lexerToNfa' rules = do
  start <- freshState
  for_ rules \rule -> do
    ruleToNfa start rule
  pure start

ruleToNfa :: MonadState (NfaBuilder a) m => Nfa.StateId -> Rule a -> m ()
ruleToNfa from Rule {regex, accept} = do
  to <- freshStateWith Nfa.defState {Nfa.accept = Just accept}
  regexToNfa' from to regex

regexToNfa :: a -> RE.Regex -> Nfa a
regexToNfa accept regex = Nfa {Nfa.start, Nfa.states = VB.fromListN (PVec.length nfa) (PVec.toList nfa)}
  where
    (start, NfaBuilder {nfa}) = flip State.runState newNfaBuilder do
      s1 <- freshState
      s2 <- freshStateWith Nfa.defState {Nfa.accept = Just accept}
      regexToNfa' s1 s2 regex
      pure s1

regexToNfa' :: MonadNfa a m => Int -> Int -> RE.Regex -> m ()
regexToNfa' from to = \case
  -- RE.Empty -> emptyEdge from to
  RE.Cat rs -> catToNfa from to rs regexToNfa'
  RE.Alt rs -> altToNfa from to (NE.toList rs) regexToNfa'
  -- RE.Cat r1 r2 -> do
  --   s <- freshState
  --   regexToNfa' from s r1
  --   regexToNfa' s to r2
  -- RE.Alt r1 r2 -> do
  --   regexToNfa' from to r1
  --   regexToNfa' from to r2
  RE.Set set -> charSetEdge from to set
  RE.Rep r -> do
    s <- freshState
    emptyEdge from s
    regexToNfa' s s r
    emptyEdge s to

catToNfa :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> [r] -> (Nfa.StateId -> Nfa.StateId -> r -> m ()) -> m ()
catToNfa from to rs toNfa = do
  from <-
    foldlM
      ( \from r -> do
          s <- freshState
          toNfa from s r
          pure s
      )
      from
      rs
  emptyEdge from to

altToNfa :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> [r] -> (Nfa.StateId -> Nfa.StateId -> r -> m ()) -> m ()
altToNfa from to rs toNfa = for_ rs $ toNfa from to

newNfaBuilder :: NfaBuilder a
newNfaBuilder = NfaBuilder {nfa = mempty, next = 0}

charSetToUtf8Sequences :: CharSet -> [Utf8Sequence]
charSetToUtf8Sequences = concatMap (utf8Sequences . uncurry (...) . both Char.ord) . CharSet.toRangeList

charSetEdge :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> CharSet -> m ()
charSetEdge from to set = do
  altToNfa from to (charSetToUtf8Sequences set) utf8SequenceEdge
  setIsCharEnd to

utf8SequenceEdge :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> Utf8Sequence -> m ()
utf8SequenceEdge from to sequence = catToNfa from to (Foldable.toList sequence) utf8RangeEdge

utf8RangeEdge :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> Utf8Range -> m ()
utf8RangeEdge from to utf8Range = do
  modifyState from \state ->
    state {Nfa.transitions = (utf8Range, to) : Nfa.transitions state}

emptyEdge :: MonadNfa a m => Int -> Int -> m ()
emptyEdge from to =
  modifyState from \state ->
    state {Nfa.emptyTransitions = IntSet.insert to $ Nfa.emptyTransitions state}

addState :: Nfa.State a -> NfaBuilder a -> (Int, NfaBuilder a)
addState s NfaBuilder {nfa, next} = (next, NfaBuilder {nfa = PVec.snoc nfa s, next = next + 1})

freshStateWith :: MonadNfa a m => Nfa.State a -> m Int
freshStateWith s = do
  builder <- State.get
  let (i, builder') = addState s builder
  State.put $! builder'
  pure i

freshState :: MonadNfa a m => m Int
freshState = freshStateWith Nfa.defState

setIsCharEnd :: MonadNfa a m => Nfa.StateId -> m ()
setIsCharEnd s = modifyState s \state -> state {Nfa.isCharEnd = True}

modifyState :: MonadNfa a m => Nfa.StateId -> (Nfa.State a -> Nfa.State a) -> m ()
modifyState s f = State.modify' \builder@NfaBuilder {nfa} ->
  builder
    { nfa =
        PVec.adjust
          f
          s
          nfa
    }
