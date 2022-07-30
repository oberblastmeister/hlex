module Text.HLex.Internal.RegexToNfa
  ( lexerToNfa,
  )
where

import Control.Monad (forM_)
import Control.Monad.State.Strict (MonadState)
import Control.Monad.State.Strict qualified as State
import Data.HashSet qualified as HashSet
import Data.Vector qualified as VB
import Data.Vector.Persistent qualified as PVec
import Data.Word (Word8)
import Language.Haskell.TH (Code, Q)
import Text.HLex.Internal.Lexer (Lexer (Lexer))
import Text.HLex.Internal.Lexer qualified as Lexer
import Text.HLex.Internal.Nfa (Nfa (Nfa))
import Text.HLex.Internal.Nfa qualified as Nfa
import Text.HLex.Internal.Range (Range)
import Text.HLex.Internal.Regex.Core qualified as Core

data NfaBuilder a = NfaBuilder
  { nfa :: !(PVec.Vector (Nfa.State a)),
    next :: !Int
  }

lexerToNfa :: Lexer a -> Nfa (Code Q a)
lexerToNfa lexer =
  Nfa
    { Nfa.starts = [start],
      Nfa.states = VB.fromListN (PVec.length nfa) (PVec.toList nfa)
    }
  where
    (start, NfaBuilder {nfa}) = State.runState (lexerToNfa' lexer) newNfaBuilder

lexerToNfa' :: MonadState (NfaBuilder (Code Q a)) m => Lexer a -> m Int
lexerToNfa' Lexer {Lexer.rules} = do
  start <- freshState
  forM_ rules $ \rule -> do
    (s1, _s2) <- ruleToNfa rule
    addEmptyTransition start s1
  pure start

ruleToNfa :: MonadState (NfaBuilder (Code Q a)) m => Lexer.Rule a -> m (Int, Int)
ruleToNfa Lexer.Rule {Lexer.regex, Lexer.accept} = do
  s1 <- freshState
  s2 <- freshStateWith Nfa.defState {Nfa.accept = Just accept}
  regexToNfa s1 s2 regex
  pure (s1, s2)

regexToNfa :: MonadState (NfaBuilder a) m => Int -> Int -> Core.Regex -> m ()
regexToNfa a b = \case
  Core.Empty -> addEmptyTransition a b
  Core.Concat r1 r2 -> do
    s <- freshState
    regexToNfa a s r1
    regexToNfa s b r2
  Core.Alt r1 r2 -> do
    regexToNfa a b r1
    regexToNfa a b r2
  Core.Range start end -> 
    undefined
    -- addByteRangeTransition a b range
  Core.Rep r -> do
    s <- freshState
    addEmptyTransition a s
    regexToNfa s s r
    addEmptyTransition s b

newNfaBuilder :: NfaBuilder a
newNfaBuilder = NfaBuilder {nfa = mempty, next = 0}

addByteRangeTransition :: MonadState (NfaBuilder a) m => Int -> Int -> Range -> m ()
addByteRangeTransition a b range = do
  State.modify' $ \builder@NfaBuilder {nfa} ->
    builder
      { nfa =
          PVec.adjust
            (\st@Nfa.State {Nfa.transitions} -> st {Nfa.transitions = (range, b) : transitions})
            a
            nfa
      }

addEmptyTransition :: MonadState (NfaBuilder a) m => Int -> Int -> m ()
addEmptyTransition a b = do
  State.modify' $ \(builder@NfaBuilder {nfa}) ->
    -- oof, this is why lenses exist
    builder
      { nfa =
          PVec.adjust
            ( \st@Nfa.State {emptyTransitions} ->
                st {Nfa.emptyTransitions = HashSet.insert b emptyTransitions}
            )
            a
            nfa
      }

addState :: Nfa.State a -> NfaBuilder a -> (Int, NfaBuilder a)
addState s NfaBuilder {nfa, next} = (next, NfaBuilder {nfa = PVec.snoc nfa s, next = next + 1})

freshStateWith :: MonadState (NfaBuilder a) m => Nfa.State a -> m Int
freshStateWith s = do
  builder <- State.get
  let (i, builder') = addState s builder
  State.put $! builder'
  pure i

freshState :: MonadState (NfaBuilder a) m => m Int
freshState = freshStateWith Nfa.defState
