{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedRecordDot #-}

module Ilex.Internal.Monad where

import Control.Monad.State (MonadState (..))
import Data.ByteString (ByteString)
import Data.Primitive (ByteArray#)
import Data.Primitive qualified as Primitive
import Data.Text (Text)
import Data.Text.Array qualified
import Data.Text.Internal qualified
import GHC.Exts (Int (..), Int#, (+#), (-#))
import GHC.Exts qualified as Exts
import Ilex.Internal.ByteString (unpackByteString)

newtype LexerInput# = LexerInput## (# ByteArray#, Int#, LexerState#, LexerState# #)

pattern LexerInput# :: ByteArray# -> Int# -> LexerState# -> LexerState# -> LexerInput#
pattern LexerInput# {inputArr#, inputArrOff#, inputStart#, inputEnd#} = LexerInput## (# inputArr#, inputArrOff#, inputStart#, inputEnd# #)

{-# COMPLETE LexerInput# #-}

data Pos = Pos
  { bytePos :: !Int,
    charPos :: !Int
  }

data LexerInput = LI LexerInput#

inputText :: LexerInput -> Text
inputText
  ( LI
      LexerInput#
        { inputArr#,
          inputStart# = LexerState {off#},
          inputEnd# = LexerState {off# = endOff#}
        }
    ) =
    Data.Text.Internal.Text
      (Data.Text.Array.ByteArray inputArr#)
      (I# off#)
      (I# (endOff# -# off#))
{-# INLINE inputText #-}

inputStart :: LexerInput -> Pos
inputStart
  ( LI
      LexerInput#
        { inputArrOff#,
          inputStart# = LexerState {off#, charOff#}
        }
    ) =
    Pos {bytePos = I# (off# -# inputArrOff#), charPos = I# charOff#}
{-# INLINE inputStart #-}

inputEnd :: LexerInput -> Pos
inputEnd
  ( LI
      LexerInput#
        { inputArrOff#,
          inputEnd# = LexerState {off#, charOff#}
        }
    ) =
    Pos {bytePos = I# (off# -# inputArrOff#), charPos = I# charOff#}
{-# INLINE inputEnd #-}

newtype LexerEnv# = LexerEnv# (# ByteArray#, Int#, Int# #)

pattern LexerEnv :: ByteArray# -> Int# -> Int# -> LexerEnv#
pattern LexerEnv {arr#, endOff#, arrOff#} = LexerEnv# (# arr#, endOff#, arrOff# #)

{-# COMPLETE LexerEnv #-}

newtype LexerState# = LexerState# (# Int#, Int# #)

pattern LexerState :: Int# -> Int# -> LexerState#
pattern LexerState {off#, charOff#} = LexerState# (# off#, charOff# #)

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
  withLexerEnv :: (LexerEnv# -> m a) -> m a
  withLexerState :: (LexerState# -> m a) -> m a
  setLexerState :: LexerState# -> m ()

getCharPos :: MonadLexer m => m Int
getCharPos = withLexerState \LexerState {charOff#} -> pure (I# charOff#)
{-# INLINE getCharPos #-}

getBytePos :: MonadLexer m => m Int
getBytePos = withLexerState \LexerState {off#} -> pure (I# off#)
{-# INLINE getBytePos #-}

runLexText :: Text -> s -> Lex s a -> (s, a)
runLexText (Data.Text.Internal.Text (Data.Text.Array.ByteArray bs) (I# off#) (I# len#)) s (Lex f) =
  case f (LexerEnv {arr# = bs, endOff# = off# +# len#, arrOff# = off#}) (LexerState {off# = off#, charOff# = 0#}) s of
    (# _, s, a #) -> (s, a)

runLexByteString :: ByteString -> s -> Lex s a -> (s, a)
runLexByteString bytestring s (Lex f) =
  case f (LexerEnv {arr# = bs, endOff# = endOff#, arrOff# = off#}) (LexerState {off# = off#, charOff# = 0#}) s of
    (# _, s, a #) -> (s, a)
  where
    !(Primitive.ByteArray bs, I# off#, I# endOff#) = unpackByteString bytestring

newtype Lex s a = Lex' {unLex :: LexerEnv# -> LexerState# -> s -> (# LexerState#, s, a #)}

pattern Lex :: (LexerEnv# -> LexerState# -> s -> (# LexerState#, s, a #)) -> Lex s a
pattern Lex f <- Lex' f
  where
    Lex f = Lex' $ Exts.oneShot \le -> Exts.oneShot \ls -> Exts.oneShot \s -> f le ls s

{-# COMPLETE Lex #-}

instance Functor (Lex s) where
  fmap f (Lex m) = Lex \env ls s -> case m env ls s of
    (# ls, s, a #) -> (# ls, s, f a #)
  {-# INLINE fmap #-}

instance Applicative (Lex s) where
  pure a = Lex \_ ls s -> (# ls, s, a #)
  {-# INLINE pure #-}
  Lex f <*> Lex a = Lex \env ls s -> case f env ls s of
    (# ls, s, f' #) -> case a env ls s of
      (# ls, s, a' #) -> (# ls, s, f' a' #)
  {-# INLINE (<*>) #-}

instance Monad (Lex s) where
  Lex m >>= f = Lex \env ls s -> case m env ls s of
    (# ls, s, a #) -> unLex (f a) env ls s
  {-# INLINE (>>=) #-}

instance MonadState s (Lex s) where
  get = Lex \_ ls s -> (# ls, s, s #)
  {-# INLINE get #-}
  put s = Lex \_ ls _ -> (# ls, s, () #)
  {-# INLINE put #-}

instance MonadLexer (Lex s) where
  withLexerEnv f = Lex \env ls s -> unLex (f env) env ls s
  {-# INLINE withLexerEnv #-}
  withLexerState f = Lex \env ls s -> unLex (f ls) env ls s
  {-# INLINE withLexerState #-}
  setLexerState ls = Lex \_env _ s -> (# ls, s, () #)
  {-# INLINE setLexerState #-}
