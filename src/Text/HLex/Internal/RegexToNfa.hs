module Text.HLex.Internal.RegexToNfa
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
import Data.Vector qualified as VB
import Data.Vector.Persistent qualified as PVec
import Numeric.Interval.NonEmpty ((...))
import Text.HLex.Internal.CharSet (CharSet)
import Text.HLex.Internal.CharSet qualified as CharSet
import Text.HLex.Internal.Lexer (Lexer (Lexer))
import Text.HLex.Internal.Lexer qualified as Lexer
import Text.HLex.Internal.Nfa (Nfa (Nfa))
import Text.HLex.Internal.Nfa qualified as Nfa
import Text.HLex.Internal.Regex qualified as RE
import Text.HLex.Internal.Utf8
import Text.HLex.Internal.Utils

data NfaBuilder a = NfaBuilder
  { nfa :: !(PVec.Vector (Nfa.State a)),
    next :: !Nfa.StateId
  }

type MonadNfa a = MonadState (NfaBuilder a)

lexerToNfa :: Lexer a -> Nfa (Lexer.Accept a)
lexerToNfa lexer =
  Nfa
    { Nfa.start,
      Nfa.states = VB.fromListN (PVec.length nfa) (PVec.toList nfa)
    }
  where
    (start, NfaBuilder {nfa}) = State.runState (lexerToNfa' lexer) newNfaBuilder

lexerToNfa' :: MonadState (NfaBuilder (Lexer.Accept a)) m => Lexer a -> m Int
lexerToNfa' Lexer {Lexer.rules} = do
  start <- freshState
  for_ rules $ \rule -> do
    (s1, _s2) <- ruleToNfa rule
    -- need to connect this to the end
    emptyEdge start s1
  pure start

ruleToNfa :: MonadState (NfaBuilder (Lexer.Accept a)) m => Lexer.Rule a -> m (Int, Int)
ruleToNfa Lexer.Rule {Lexer.regex, Lexer.accept} = do
  s1 <- freshState
  s2 <- freshStateWith Nfa.defState {Nfa.accept = Just accept}
  regexToNfa' s1 s2 regex
  pure (s1, s2)

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
  RE.Empty -> emptyEdge from to
  RE.Cat r1 r2 -> do
    s <- freshState
    regexToNfa' from s r1
    regexToNfa' s to r2
  RE.Alt r1 r2 -> do
    regexToNfa' from to r1
    regexToNfa' from to r2
  RE.Set set -> charSetEdge from to set
  RE.Rep r -> do
    s <- freshState
    emptyEdge from s
    regexToNfa' s s r
    emptyEdge s to

newNfaBuilder :: NfaBuilder a
newNfaBuilder = NfaBuilder {nfa = mempty, next = 0}

charSetToUtf8Sequences :: CharSet -> [Utf8Sequence]
charSetToUtf8Sequences = concatMap (utf8Sequences . uncurry (...) . both Char.ord) . CharSet.toRangeList

charSetEdge :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> CharSet -> m ()
charSetEdge from to set = do
  for_ (charSetToUtf8Sequences set) $ utf8SequenceEdge from to

utf8SequenceEdge :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> Utf8Sequence -> m ()
utf8SequenceEdge from to sequence = do
  from <-
    foldlM
      ( \from utf8Range -> do
          s <- freshState
          utf8RangeEdge from s utf8Range
          pure s
      )
      from
      (Foldable.toList sequence)
  emptyEdge from to

utf8RangeEdge :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> Utf8Range -> m ()
utf8RangeEdge from to utf8Range = do
  State.modify' $ \builder@NfaBuilder {nfa} -> do
    builder
      { nfa =
          PVec.adjust
            (\st@Nfa.State {Nfa.transitions} -> st {Nfa.transitions = (utf8Range, to) : transitions})
            from
            nfa
      }

emptyEdge :: MonadNfa a m => Int -> Int -> m ()
emptyEdge from to = do
  State.modify' $ \builder@NfaBuilder {nfa} ->
    -- oof, this is why lenses exist
    builder
      { nfa =
          PVec.adjust
            ( \st@Nfa.State {Nfa.emptyTransitions} ->
                st {Nfa.emptyTransitions = IntSet.insert to emptyTransitions}
            )
            from
            nfa
      }

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
