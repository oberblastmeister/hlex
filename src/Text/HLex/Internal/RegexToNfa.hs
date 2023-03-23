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
import Debug.Trace
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

-- toUtf8Range :: ([Byte], [Byte]) -> [([Byte], [Byte])]
-- toUtf8Range (x, y)
--   | lenX > lenY = error "toSameLengthUtf8Range: first range is longer than second"
--   | lenX == lenY = [(x, y)]
--   | lenX == 1 = (x, [0x7f]) : toUtf8Range ([0xc2, 0x80], y)
--   | lenX == 2 = (x, [0xdf, 0xbf]) : toUtf8Range ([0xe0, 0xa0, 0x80], y)
--   | lenX == 3 = (x, [0xef, 0xbf, 0xbf]) : toUtf8Range ([0xf0, 0x90, 0x80, 0x80], y)
--   | otherwise = error "toSameLengthUtf8Range: invalid utf8"
--   where
--     lenX = length x
--     lenY = length y

-- encodeCharUtf8 :: Char -> [Word8]
-- encodeCharUtf8 = B.unpack . T.encodeUtf8 . T.singleton

-- charSetToUtf8Ranges :: CharSet -> [([Byte], [Byte])]
-- charSetToUtf8Ranges = concatMap (toUtf8Range . both encodeCharUtf8) . CharSet.toRangeList
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
          let !_ = traceId $ "s: " ++ show s
          utf8RangeEdge from s utf8Range
          pure s
      )
      from
      (Foldable.toList sequence)
  let !_ = traceId $ "from: " ++ show from
  emptyEdge from to

utf8RangeEdge :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> Utf8Range -> m ()
utf8RangeEdge from to utf8Range = do
  let !_ = traceId $ "utf8RangeEdge: " ++ show (from, to, utf8Range)
  State.modify' $ \builder@NfaBuilder {nfa} -> do
    let !_ = traceId $ "len: " ++ show (length nfa)
    builder
      { nfa =
          PVec.adjust
            (\st@Nfa.State {Nfa.transitions} -> st {Nfa.transitions = (utf8Range, to) : transitions})
            from
            nfa
      }

-- utf8RangeEdge :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> ([Byte], [Byte]) -> m ()
-- utf8RangeEdge from to ([], []) = emptyEdge from to
-- utf8RangeEdge from to ([x], [y]) = byteSetEdge from to $ RSet.singletonRange (x, y)
-- utf8RangeEdge from to (x : xs, y : ys)
--   | x == y = do
--       s <- freshState
--       byteSetEdge from s $ RSet.singleton x
--       utf8RangeEdge s to (xs, ys)
--   | x < y = do
--       do
--         s <- freshState
--         byteSetEdge from s $ RSet.singleton x
--         -- this is definately wrong, 0xff may not be valid utf8
--         utf8RangeEdge s to (xs, 0xff <$ ys)
--       do
--         s <- freshState
--         byteSetEdge from s $ RSet.singleton y
--         utf8RangeEdge s to (0x00 <$ xs, ys)
--       when (x + 1 <= y - 1) do
--         s <- freshState
--         byteSetEdge from s $ RSet.singletonRange (x + 1, y - 1)
--         anyBytesEdge s to $ length xs
-- utf8RangeEdge _ _ _ = error "utf8RangeEdge: invalid utf8 range"

-- utf8RangeToBytesRanges :: ([Byte], [Byte]) -> [[(Byte, Byte)]]
-- utf8RangeToBytesRanges ([], []) = []
-- utf8RangeToBytesRanges ([x], [y]) = [[(x, y)]]
-- utf8RangeToBytesRanges (x : xs, y : ys)
--   | x == y = [(x, x)] : utf8RangeToBytesRanges (xs, ys)
--   | x < y = (x, 0xff) : (0x00, y) : utf8RangeToBytesRanges (xs, ys)
--   | otherwise = error "utf8RangeToBytesRanges: invalid utf8 range"
-- utf8RangeToBytesRanges ([], []) = []
-- utf8RangeToBytesRanges ([x], [y]) = [(x, y)]
-- utf8RangeToBytesRanges (x : xs, y : ys)
--   | x == y =
--   | x < y = (x, 0xff) : (0x00, y) : utf8RangeToBytesRanges (xs, ys)
--   | otherwise = error "utf8RangeToBytesRanges: invalid utf8 range"

-- anyBytesEdge :: MonadNfa a m => Nfa.StateId -> Nfa.StateId -> Int -> m ()
-- anyBytesEdge from to n = do
--   s <-
--     foldlM
--       ( \from _ -> do
--           s <- freshState
--           utf8RangeEdge from s $ RSet.singletonRange (0x00, 0xff)
--           pure s
--       )
--       from
--       [1 .. n]
--   emptyEdge s to

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
