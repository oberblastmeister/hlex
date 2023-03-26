{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedRecordDot #-}

module Ilex.Internal.Monad where

import Control.Monad.State (MonadState (..))
import Data.Primitive (ByteArray#)
import GHC.Exts (Int (..), Int#)

newtype LexerEnv = LexerEnv# (# ByteArray#, Int#, Int# #)

pattern LexerEnv :: ByteArray# -> Int# -> Int# -> LexerEnv
pattern LexerEnv {arr, len, arrOff} = LexerEnv# (# arr, len, arrOff #)

{-# COMPLETE LexerEnv #-}

newtype LexerState = LexerState# (# Int# #)

pattern LexerState :: Int# -> LexerState
pattern LexerState {off} = LexerState# (# off #)

{-# COMPLETE LexerState #-}

newtype LineCol# = LineCol## (# Int#, Int# #)

pattern LineCol# :: Int# -> Int# -> LineCol#
pattern LineCol# {line#, col#} = LineCol## (# line#, col# #)

{-# COMPLETE LineCol# #-}

data LineCol = LineCol
  { line :: !Int,
    col :: !Int
  }

liftLineCol :: LineCol# -> LineCol
liftLineCol (LineCol# {line#, col#}) = LineCol {line = I# line#, col = I# col#}
{-# INLINE liftLineCol #-}

class Monad m => MonadLexer m where
  withLexerEnv :: (LexerEnv -> m a) -> m a
  withLexerState :: (LexerState -> m a) -> m a
  setLexerState :: LexerState -> m ()

newtype Lexer s a = Lexer {runLexer :: LexerEnv -> LexerState -> s -> (# LexerState, s, a #)}

instance Functor (Lexer s) where
  fmap f (Lexer m) = Lexer \env ls s -> case m env ls s of
    (# ls, s, a #) -> (# ls, s, f a #)
  {-# INLINE fmap #-}

instance Applicative (Lexer s) where
  pure a = Lexer \_ ls s -> (# ls, s, a #)
  {-# INLINE pure #-}
  Lexer f <*> Lexer a = Lexer \env ls s -> case f env ls s of
    (# ls, s, f' #) -> case a env ls s of
      (# ls, s, a' #) -> (# ls, s, f' a' #)
  {-# INLINE (<*>) #-}

instance Monad (Lexer s) where
  Lexer m >>= f = Lexer \env ls s -> case m env ls s of
    (# ls, s, a #) -> runLexer (f a) env ls s
  {-# INLINE (>>=) #-}

instance MonadState s (Lexer s) where
  get = Lexer \_ ls s -> (# ls, s, s #)
  {-# INLINE get #-}
  put s = Lexer \_ ls _ -> (# ls, s, () #)
  {-# INLINE put #-}

instance MonadLexer (Lexer s) where
  withLexerEnv f = Lexer \env ls s -> runLexer (f env) env ls s
  {-# INLINE withLexerEnv #-}
  withLexerState f = Lexer \env ls s -> runLexer (f ls) env ls s
  {-# INLINE withLexerState #-}
  setLexerState ls = Lexer \_env _ s -> (# ls, s, () #)
  {-# INLINE setLexerState #-}
