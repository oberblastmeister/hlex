module Hlex.Internal.Prim
  ( sameByteArray,
    sameByteArray#,
    unI#,
  )
where

import Data.Primitive (ByteArray (..))
import GHC.Exts (ByteArray#, Int (..), Int#, isTrue#, sameMutableByteArray#)
import Unsafe.Coerce (unsafeCoerce#)

sameByteArray :: ByteArray -> ByteArray -> Bool
sameByteArray (ByteArray bs#) (ByteArray bs##) = isTrue# (sameByteArray# bs# bs##)
{-# INLINE sameByteArray #-}

sameByteArray# :: ByteArray# -> ByteArray# -> Int#
sameByteArray# bs# bs## = sameMutableByteArray# (unsafeCoerce# bs#) (unsafeCoerce# bs##)
{-# INLINE sameByteArray# #-}

unI# :: Int -> Int#
unI# (I# i) = i
{-# INLINE unI# #-}
