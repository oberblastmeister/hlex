{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedRecordDot #-}

module Ilex.Internal.Monad where

import Control.Monad.Cont qualified as Cont
import Control.Monad.Except qualified as Except
import Control.Monad.Reader qualified as Reader
import Control.Monad.State (MonadState (..))
import Control.Monad.State.Lazy qualified as Lazy
import Control.Monad.State.Strict qualified as Strict
import Data.ByteString (ByteString)
import Data.Functor (($>))
import Data.Primitive (ByteArray#)
import Data.Primitive qualified as Primitive
import Data.Text (Text)
import Data.Text.Array qualified
import Data.Text.Internal qualified
import GHC.Exts (Int (..), Int#, (+#), (-#))
import GHC.Exts qualified as Exts
import GHC.Generics (Generic)
import Ilex.Internal.ByteString (unpackByteString)

newtype LexerInput# = LexerInput## (# ByteArray#, Int#, LexerState#, LexerState# #)

pattern LexerInput# :: ByteArray# -> Int# -> LexerState# -> LexerState# -> LexerInput#
pattern LexerInput# {inputArr#, inputArrOff#, inputStart#, inputEnd#} = LexerInput## (# inputArr#, inputArrOff#, inputStart#, inputEnd# #)

{-# COMPLETE LexerInput# #-}

data Pos = Pos
  { bytePos :: !Int,
    charPos :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

data LexerInput = LI LexerInput#

getPos :: MonadLexer m => m Pos
getPos = withLexerState \LexerState {off#, charOff#} -> do
  withLexerEnv \LexerEnv {arrOff#} -> do
    let bytePos = I# (off# -# arrOff#)
    let charPos = I# charOff#
    pure Pos {bytePos, charPos}
{-# INLINE getPos #-}

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

instance MonadLexer m => MonadLexer (Lazy.StateT s m) where
  withLexerEnv f = Lazy.StateT \s -> withLexerEnv \le -> Lazy.runStateT (f le) s
  withLexerState f = Lazy.StateT \s -> withLexerState \ls -> Lazy.runStateT (f ls) s
  setLexerState ls = Lazy.StateT \s -> setLexerState ls $> ((), s)
  {-# INLINE withLexerEnv #-}
  {-# INLINE withLexerState #-}
  {-# INLINE setLexerState #-}

instance MonadLexer m => MonadLexer (Strict.StateT s m) where
  withLexerEnv f = Strict.StateT \s -> withLexerEnv \le -> Strict.runStateT (f le) s
  withLexerState f = Strict.StateT \s -> withLexerState \ls -> Strict.runStateT (f ls) s
  setLexerState ls = Strict.StateT \s -> setLexerState ls $> ((), s)
  {-# INLINE withLexerEnv #-}
  {-# INLINE withLexerState #-}
  {-# INLINE setLexerState #-}

instance MonadLexer m => MonadLexer (Except.ExceptT e m) where
  withLexerEnv f = Except.ExceptT $ withLexerEnv \le -> Except.runExceptT $ f le
  withLexerState f = Except.ExceptT $ withLexerState \ls -> Except.runExceptT $ f ls
  setLexerState ls = Except.ExceptT $ setLexerState ls $> Right ()
  {-# INLINE withLexerEnv #-}
  {-# INLINE withLexerState #-}
  {-# INLINE setLexerState #-}

instance MonadLexer m => MonadLexer (Reader.ReaderT r m) where
  withLexerEnv f = Reader.ReaderT \r -> withLexerEnv \le -> Reader.runReaderT (f le) r
  withLexerState f = Reader.ReaderT \r -> withLexerState \ls -> Reader.runReaderT (f ls) r
  setLexerState ls = Reader.ReaderT \_ -> setLexerState ls
  {-# INLINE withLexerEnv #-}
  {-# INLINE withLexerState #-}
  {-# INLINE setLexerState #-}

instance MonadLexer m => MonadLexer (Cont.ContT r m) where
  withLexerEnv f = Cont.ContT \c -> withLexerEnv \le -> Cont.runContT (f le) c
  withLexerState f = Cont.ContT \c -> withLexerState \ls -> Cont.runContT (f ls) c
  setLexerState ls = Cont.ContT \c -> setLexerState ls *> c ()
  {-# INLINE withLexerEnv #-}
  {-# INLINE withLexerState #-}
  {-# INLINE setLexerState #-}

getCharPos :: MonadLexer m => m Int
getCharPos = withLexerState \LexerState {charOff#} -> pure (I# charOff#)
{-# INLINE getCharPos #-}

getBytePos :: MonadLexer m => m Int
getBytePos = withLexerState \LexerState {off#} -> pure (I# off#)
{-# INLINE getBytePos #-}

lexText :: Lex s a -> Text -> s -> (s, a)
lexText (Lex f) (Data.Text.Internal.Text (Data.Text.Array.ByteArray bs) (I# off#) (I# len#)) s =
  case f (LexerEnv {arr# = bs, endOff# = off# +# len#, arrOff# = off#}) (LexerState {off# = off#, charOff# = 0#}) s of
    (# _, s, a #) -> (s, a)

lexByteString :: Lex s a -> ByteString -> s -> (s, a)
lexByteString (Lex f) bytestring s =
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
