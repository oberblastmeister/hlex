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

data Input (us :: Utf8Status) = Input
  { inputArr :: !Primitive.ByteArray,
    inputStart :: !Int,
    inputEnd :: !Int
  }

inputLength :: Input us -> Int
inputLength Input {inputStart, inputEnd} = inputEnd - inputStart
{-# INLINE inputLength #-}

instance Show (Input IsUtf8) where
  show = show . inputText

instance Eq (Input us) where
  i == i' = inputLength i == inputLength i' && compare i i' == EQ

instance Ord (Input us) where
  compare i i' = Primitive.compareByteArrays (inputArr i) (inputStart i) (inputArr i') (inputStart i') (min (inputLength i) (inputLength i'))

type Utf8Input = Input IsUtf8

type BytesInput = Input UnknownUtf8

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
getPos = withPos \Pos# {off#, charOff#} -> do
  withEnv \Env# {arrOff#} -> do
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

mergeInput :: Input IsUtf8 -> Input IsUtf8 -> Input IsUtf8
mergeInput
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
{-# INLINE mergeInput #-}

newtype Env# = Env## (# ByteArray#, Int#, Int# #)

pattern Env# :: ByteArray# -> Int# -> Int# -> Env#
pattern Env# {arr#, endOff#, arrOff#} = Env## (# arr#, endOff#, arrOff# #)

{-# COMPLETE Env# #-}

newtype Pos# = Pos## (# Int#, Int# #)

pattern Pos# :: Int# -> Int# -> Pos#
pattern Pos# {off#, charOff#} = Pos## (# off#, charOff# #)

{-# COMPLETE Pos# #-}

class Monad m => MonadLexer m where
  withEnv :: (Env# -> m a) -> m a
  withPos :: (Pos# -> m a) -> m a
  setPos :: Pos# -> m ()

instance MonadLexer m => MonadLexer (Lazy.StateT s m) where
  withEnv f = Lazy.StateT \s -> withEnv \le -> Lazy.runStateT (f le) s
  withPos f = Lazy.StateT \s -> withPos \ls -> Lazy.runStateT (f ls) s
  setPos ls = Lazy.StateT \s -> setPos ls $> ((), s)
  {-# INLINE withEnv #-}
  {-# INLINE withPos #-}
  {-# INLINE setPos #-}

instance MonadLexer m => MonadLexer (Strict.StateT s m) where
  withEnv f = Strict.StateT \s -> withEnv \le -> Strict.runStateT (f le) s
  withPos f = Strict.StateT \s -> withPos \ls -> Strict.runStateT (f ls) s
  setPos ls = Strict.StateT \s -> setPos ls $> ((), s)
  {-# INLINE withEnv #-}
  {-# INLINE withPos #-}
  {-# INLINE setPos #-}

instance MonadLexer m => MonadLexer (Except.ExceptT e m) where
  withEnv f = Except.ExceptT $ withEnv \le -> Except.runExceptT $ f le
  withPos f = Except.ExceptT $ withPos \ls -> Except.runExceptT $ f ls
  setPos ls = Except.ExceptT $ setPos ls $> Right ()
  {-# INLINE withEnv #-}
  {-# INLINE withPos #-}
  {-# INLINE setPos #-}

instance MonadLexer m => MonadLexer (Reader.ReaderT r m) where
  withEnv f = Reader.ReaderT \r -> withEnv \le -> Reader.runReaderT (f le) r
  withPos f = Reader.ReaderT \r -> withPos \ls -> Reader.runReaderT (f ls) r
  setPos ls = Reader.ReaderT \_ -> setPos ls
  {-# INLINE withEnv #-}
  {-# INLINE withPos #-}
  {-# INLINE setPos #-}

instance MonadLexer m => MonadLexer (Cont.ContT r m) where
  withEnv f = Cont.ContT \c -> withEnv \le -> Cont.runContT (f le) c
  withPos f = Cont.ContT \c -> withPos \ls -> Cont.runContT (f ls) c
  setPos ls = Cont.ContT \c -> setPos ls *> c ()
  {-# INLINE withEnv #-}
  {-# INLINE withPos #-}
  {-# INLINE setPos #-}

getRemainingInput :: MonadLexer m => m (Input UnknownUtf8)
getRemainingInput = withEnv \Env# {arr#, endOff#} -> do
  withPos \Pos# {off#} ->
    pure $ unknownUtf8Input Input# {inputArr# = arr#, inputStart# = off#, inputEnd# = endOff#}
{-# INLINE getRemainingInput #-}

getCharPos :: MonadLexer m => m Int
getCharPos = withPos \Pos# {charOff#} -> pure (I# charOff#)
{-# INLINE getCharPos #-}

getBytePos :: MonadLexer m => m Int
getBytePos = withPos \Pos# {off#} -> pure (I# off#)
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
  withEnv f = Lex \env ls s -> unLex (f env) env ls s
  {-# INLINE withEnv #-}
  withPos f = Lex \env ls s -> unLex (f ls) env ls s
  {-# INLINE withPos #-}
  setPos ls = Lex \_env _ s -> (# ls, s, () #)
  {-# INLINE setPos #-}
