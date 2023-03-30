{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE NoDuplicateRecordFields #-}
{-# LANGUAGE NoOverloadedRecordDot #-}

module Ilex.Internal.Monad
  ( Input# (Input#, inputArr#, inputStart#, inputEnd#, ..),
    Pos (..),
    Utf8Status (..),
    Input (..),
    Lex,
    isInputStart,
    isInputEnd,
    inputLength,
    Utf8Input,
    BytesInput,
    liftInput#,
    unliftInput#,
    getPos,
    inputFromText,
    inputFromByteString,
    inputText,
    spanInput,
    Pos# (Pos#, off#, charOff#, ..),
    defPos#,
    MonadLexer (..),
    getRemainingInput,
    lexInput,
    lexText,
    lexByteString,
  )
where

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
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Array qualified
import Data.Text.Internal qualified
import GHC.Exts (Int (..), Int#, (-#))
import GHC.Exts qualified as Exts
import GHC.Generics (Generic)
import Ilex.Internal.ByteString (unpackByteString)
import Ilex.Internal.Prim (sameByteArray, unI#)

newtype Input# (u :: Utf8Status) = Input## (# ByteArray#, Int#, Int# #)

pattern Input# :: ByteArray# -> Int# -> Int# -> Input# u
pattern Input# {inputArr#, inputStart#, inputEnd#} = Input## (# inputArr#, inputStart#, inputEnd# #)

{-# COMPLETE Input# #-}

data Pos = Pos
  { bytePos :: !Int,
    charPos :: !Int
  }
  deriving (Show, Eq, Ord, Generic)

data Utf8Status = Utf8 | Bytes

type role Input nominal

data Input (us :: Utf8Status) = Input
  { inputArr :: !Primitive.ByteArray,
    inputStart :: !Int,
    inputEnd :: !Int
  }

isInputStart :: Input us -> Bool
isInputStart Input {inputStart} = inputStart == 0
{-# INLINE isInputStart #-}

isInputEnd :: Input us -> Bool
isInputEnd Input {inputArr, inputEnd} = inputEnd == Primitive.sizeofByteArray inputArr
{-# INLINE isInputEnd #-}

inputLength :: Input us -> Int
inputLength Input {inputStart, inputEnd} = inputEnd - inputStart
{-# INLINE inputLength #-}

instance Show (Input Utf8) where
  show = show . inputText

instance IsString (Input Utf8) where
  fromString = inputFromText . T.pack

instance Eq (Input us) where
  i == i' = inputLength i == inputLength i' && compare i i' == EQ

instance Ord (Input us) where
  compare i i' = Primitive.compareByteArrays (inputArr i) (inputStart i) (inputArr i') (inputStart i') (min (inputLength i) (inputLength i'))

type Utf8Input = Input Utf8

type BytesInput = Input Bytes

liftInput# :: Input# u -> Input u
liftInput# Input# {inputArr#, inputStart#, inputEnd#} =
  Input
    { inputArr = Primitive.ByteArray inputArr#,
      inputStart = I# inputStart#,
      inputEnd = I# inputEnd#
    }
{-# INLINE liftInput# #-}

unliftInput# :: Input u -> Input# u
unliftInput# Input {inputArr = Primitive.ByteArray inputArr#, inputStart, inputEnd} =
  Input#
    { inputArr#,
      inputStart# = unI# inputStart,
      inputEnd# = unI# inputEnd
    }
{-# INLINE unliftInput# #-}

getPos :: MonadLexer u m => m Pos
getPos = withPos \Pos# {off#, charOff#} -> do
  withInput \Input# {inputStart#} -> do
    let bytePos = I# (off# -# inputStart#)
    let charPos = I# charOff#
    pure Pos {bytePos, charPos}
{-# INLINE getPos #-}

inputFromText :: Text -> Input Utf8
inputFromText (Data.Text.Internal.Text (Data.Text.Array.ByteArray arr) off len) =
  Input
    { inputArr = Primitive.ByteArray arr,
      inputStart = off,
      inputEnd = off + len
    }
{-# INLINE inputFromText #-}

inputFromByteString :: ByteString -> Input Bytes
inputFromByteString bytestring =
  Input
    { inputArr = arr,
      inputStart = off,
      inputEnd = endOff
    }
  where
    !(arr, off, endOff) = unpackByteString bytestring
{-# INLINE inputFromByteString #-}

inputText :: Input Utf8 -> Text
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

spanInput :: Input Utf8 -> Input Utf8 -> Input Utf8
spanInput
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
{-# INLINE spanInput #-}

newtype Pos# = Pos## (# Int#, Int# #)

pattern Pos# :: Int# -> Int# -> Pos#
pattern Pos# {off#, charOff#} = Pos## (# off#, charOff# #)

{-# COMPLETE Pos# #-}

defPos# :: (# #) -> Pos#
defPos# _ = Pos# 0# 0#
{-# INLINE defPos# #-}

class Monad m => MonadLexer u m | m -> u where
  withInput :: (Input# u -> m a) -> m a
  withPos :: (Pos# -> m a) -> m a
  setPos :: Pos# -> m ()

instance MonadLexer u m => MonadLexer u (Lazy.StateT s m) where
  withInput f = Lazy.StateT \s -> withInput \le -> Lazy.runStateT (f le) s
  withPos f = Lazy.StateT \s -> withPos \ls -> Lazy.runStateT (f ls) s
  setPos ls = Lazy.StateT \s -> setPos ls $> ((), s)
  {-# INLINE withInput #-}
  {-# INLINE withPos #-}
  {-# INLINE setPos #-}

instance MonadLexer u m => MonadLexer u (Strict.StateT s m) where
  withInput f = Strict.StateT \s -> withInput \le -> Strict.runStateT (f le) s
  withPos f = Strict.StateT \s -> withPos \ls -> Strict.runStateT (f ls) s
  setPos ls = Strict.StateT \s -> setPos ls $> ((), s)
  {-# INLINE withInput #-}
  {-# INLINE withPos #-}
  {-# INLINE setPos #-}

instance MonadLexer u m => MonadLexer u (Except.ExceptT e m) where
  withInput f = Except.ExceptT $ withInput \le -> Except.runExceptT $ f le
  withPos f = Except.ExceptT $ withPos \ls -> Except.runExceptT $ f ls
  setPos ls = Except.ExceptT $ setPos ls $> Right ()
  {-# INLINE withInput #-}
  {-# INLINE withPos #-}
  {-# INLINE setPos #-}

instance MonadLexer u m => MonadLexer u (Reader.ReaderT r m) where
  withInput f = Reader.ReaderT \r -> withInput \le -> Reader.runReaderT (f le) r
  withPos f = Reader.ReaderT \r -> withPos \ls -> Reader.runReaderT (f ls) r
  setPos ls = Reader.ReaderT \_ -> setPos ls
  {-# INLINE withInput #-}
  {-# INLINE withPos #-}
  {-# INLINE setPos #-}

instance MonadLexer u m => MonadLexer u (Cont.ContT r m) where
  withInput f = Cont.ContT \c -> withInput \le -> Cont.runContT (f le) c
  withPos f = Cont.ContT \c -> withPos \ls -> Cont.runContT (f ls) c
  setPos ls = Cont.ContT \c -> setPos ls *> c ()
  {-# INLINE withInput #-}
  {-# INLINE withPos #-}
  {-# INLINE setPos #-}

getRemainingInput :: MonadLexer u m => m (Input u)
getRemainingInput = withInput \i -> withPos \Pos# {off#} ->
  pure $ liftInput# i {inputStart# = off#}
{-# INLINE getRemainingInput #-}

lexInput :: LexU u s a -> Input u -> s -> (s, a)
lexInput (Lex f) input s =
  case f (unliftInput# input) (defPos# (# #)) s of
    (# _, s, a #) -> (s, a)
{-# INLINE lexInput #-}

lexText :: Lex s a -> Text -> s -> (s, a)
lexText lex = lexInput lex . inputFromText

lexByteString :: LexU Bytes s a -> ByteString -> s -> (s, a)
lexByteString lex = lexInput lex . inputFromByteString

type Lex = LexU Utf8

newtype LexU (u :: Utf8Status) s a = Lex' {unLex :: Input# u -> Pos# -> s -> (# Pos#, s, a #)}

pattern Lex :: (Input# u -> Pos# -> s -> (# Pos#, s, a #)) -> LexU u s a
pattern Lex f <- Lex' f
  where
    Lex f = Lex' $ Exts.oneShot \le -> Exts.oneShot \ls -> Exts.oneShot \s -> f le ls s

{-# COMPLETE Lex #-}

instance Functor (LexU u s) where
  fmap f (Lex m) = Lex \env ls s -> case m env ls s of
    (# ls, s, a #) -> (# ls, s, f a #)
  {-# INLINE fmap #-}

instance Applicative (LexU u s) where
  pure a = Lex \_ ls s -> (# ls, s, a #)
  {-# INLINE pure #-}
  Lex f <*> Lex a = Lex \env ls s -> case f env ls s of
    (# ls, s, f' #) -> case a env ls s of
      (# ls, s, a' #) -> (# ls, s, f' a' #)
  {-# INLINE (<*>) #-}

instance Monad (LexU u s) where
  Lex m >>= f = Lex \env ls s -> case m env ls s of
    (# ls, s, a #) -> unLex (f a) env ls s
  {-# INLINE (>>=) #-}

instance MonadState s (LexU u s) where
  get = Lex \_ ls s -> (# ls, s, s #)
  {-# INLINE get #-}
  put s = Lex \_ ls _ -> (# ls, s, () #)
  {-# INLINE put #-}

instance MonadLexer u (LexU u s) where
  withInput f = Lex \env ls s -> unLex (f env) env ls s
  {-# INLINE withInput #-}
  withPos f = Lex \env ls s -> unLex (f ls) env ls s
  {-# INLINE withPos #-}
  setPos ls = Lex \_env _ s -> (# ls, s, () #)
  {-# INLINE setPos #-}
