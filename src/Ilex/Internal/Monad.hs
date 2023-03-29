{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
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
import Ilex.Internal.Prim (sameByteArray)

newtype Input# = Input## (# ByteArray#, Int#, Int# #)

pattern Input# :: ByteArray# -> Int# -> Int# -> Input#
pattern Input# {inputArr#, inputStart#, inputEnd#} = Input## (# inputArr#, inputStart#, inputEnd# #)

{-# COMPLETE Input# #-}

data Pos = Pos
  { bytePos :: !Int,
    charPos :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

data Utf8Status
  = IsUtf8
  | UnknownUtf8

type role Input nominal

data Input us = Input
  { inputArr :: !Primitive.ByteArray,
    inputStart :: !Int,
    inputEnd :: !Int
  }

liftInput# :: Input# -> Input us
liftInput# Input# {inputArr#, inputStart#, inputEnd#} =
  Input
    { inputArr = Primitive.ByteArray inputArr#,
      inputStart = I# inputStart#,
      inputEnd = I# inputEnd#
    }
{-# INLINE liftInput# #-}

unsafeUtf8Input :: Input# -> Input IsUtf8
unsafeUtf8Input = liftInput#
{-# INLINE unsafeUtf8Input #-}

unknownUtf8Input :: Input# -> Input UnknownUtf8
unknownUtf8Input = liftInput#
{-# INLINE unknownUtf8Input #-}

getPos :: MonadLexer m => m Pos
getPos = withLexerState \Pos# {off#, charOff#} -> do
  withLexerEnv \Env# {arrOff#} -> do
    let bytePos = I# (off# -# arrOff#)
    let charPos = I# charOff#
    pure Pos {bytePos, charPos}
{-# INLINE getPos #-}

inputText :: Input IsUtf8 -> Text
inputText
  ( Input
      { inputArr = Primitive.ByteArray inputArr#,
        inputStart,
        inputEnd
      }
    ) =
    Data.Text.Internal.Text
      (Data.Text.Array.ByteArray inputArr#)
      inputStart
      (inputEnd - inputStart)
{-# INLINE inputText #-}

combineInput :: Input IsUtf8 -> Input IsUtf8 -> Input IsUtf8
combineInput
  i1@Input
    { inputArr = inputArr1,
      inputStart = inputStart1,
      inputEnd = inputEnd1
    }
  i2@Input
    { inputArr = inputArr2,
      inputStart = inputStart2,
      inputEnd = inputEnd2
    }
    | sameByteArray inputArr1 inputArr2 =
        Input
          { inputArr = inputArr1,
            inputStart = min inputStart1 inputStart2,
            inputEnd = max inputEnd1 inputEnd2
          }
    | otherwise =
        error $
          "combineInput: cannot combine inputs sliced from different strings: "
            <> show (inputText i1, inputText i2)
{-# INLINE combineInput #-}

newtype Env# = Env## (# ByteArray#, Int#, Int# #)

pattern Env# :: ByteArray# -> Int# -> Int# -> Env#
pattern Env# {arr#, endOff#, arrOff#} = Env## (# arr#, endOff#, arrOff# #)

{-# COMPLETE Env# #-}

newtype Pos# = Pos## (# Int#, Int# #)

pattern Pos# :: Int# -> Int# -> Pos#
pattern Pos# {off#, charOff#} = Pos## (# off#, charOff# #)

{-# COMPLETE Pos# #-}

class Monad m => MonadLexer m where
  withLexerEnv :: (Env# -> m a) -> m a
  withLexerState :: (Pos# -> m a) -> m a
  setLexerState :: Pos# -> m ()

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

getRemainingInput :: MonadLexer m => m (Input UnknownUtf8)
getRemainingInput = withLexerEnv \Env# {arr#, endOff#} -> do
  withLexerState \Pos# {off#} ->
    pure $ unknownUtf8Input Input# {inputArr# = arr#, inputStart# = off#, inputEnd# = endOff#}
{-# INLINE getRemainingInput #-}

getCharPos :: MonadLexer m => m Int
getCharPos = withLexerState \Pos# {charOff#} -> pure (I# charOff#)
{-# INLINE getCharPos #-}

getBytePos :: MonadLexer m => m Int
getBytePos = withLexerState \Pos# {off#} -> pure (I# off#)
{-# INLINE getBytePos #-}

lexText :: Lex s a -> Text -> s -> (s, a)
lexText (Lex f) (Data.Text.Internal.Text (Data.Text.Array.ByteArray bs) (I# off#) (I# len#)) s =
  case f (Env# {arr# = bs, endOff# = off# +# len#, arrOff# = off#}) (Pos# {off# = off#, charOff# = 0#}) s of
    (# _, s, a #) -> (s, a)

lexByteString :: Lex s a -> ByteString -> s -> (s, a)
lexByteString (Lex f) bytestring s =
  case f (Env# {arr# = bs, endOff# = endOff#, arrOff# = off#}) (Pos# {off# = off#, charOff# = 0#}) s of
    (# _, s, a #) -> (s, a)
  where
    !(Primitive.ByteArray bs, I# off#, I# endOff#) = unpackByteString bytestring

newtype Lex s a = Lex' {unLex :: Env# -> Pos# -> s -> (# Pos#, s, a #)}

pattern Lex :: (Env# -> Pos# -> s -> (# Pos#, s, a #)) -> Lex s a
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
