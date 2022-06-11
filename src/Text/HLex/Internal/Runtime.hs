module Text.HLex.Internal.Runtime where

import Data.Primitive (Ptr)
import Data.Word (Word64)

data Table = Table
  { defaultAndOffsets :: !(Ptr Word64),
    transitions :: !(Ptr Word64)
  }
