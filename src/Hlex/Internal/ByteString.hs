{-# LANGUAGE MagicHash #-}

module Hlex.Internal.ByteString (unpackByteString) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Internal qualified as B.Internal
import Data.Primitive qualified as Primitive
import Foreign qualified
import GHC.ForeignPtr (ForeignPtr (..), ForeignPtrContents (PlainPtr))
import System.IO.Unsafe (unsafeDupablePerformIO)

unpackByteString :: ByteString -> (Primitive.ByteArray, Int, Int)
unpackByteString bs@(B.Internal.PS (ForeignPtr (Primitive.Ptr -> p) fpc) o l) =
  unsafeDupablePerformIO $ case fpc of
    PlainPtr (Primitive.MutableByteArray -> marr) -> do
      let base = Primitive.mutableByteArrayContents marr
          off = p `Foreign.minusPtr` base
      arr <- Primitive.unsafeFreezeByteArray marr
      pure (arr, off + o, off + o + l)
    _ -> case B.copy bs of
      B.Internal.PS (ForeignPtr (Primitive.Ptr -> p) fpc) o l -> case fpc of
        PlainPtr (Primitive.MutableByteArray -> marr) -> do
          let base = Primitive.mutableByteArrayContents marr
              off = p `Foreign.minusPtr` base
          arr <- Primitive.unsafeFreezeByteArray marr
          pure (arr, off + o, off + o + l)
        _ -> error "should be PlainPtr"
