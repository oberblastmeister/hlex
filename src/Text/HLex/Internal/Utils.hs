module Text.HLex.Internal.Utils where

import Data.Bifunctor (Bifunctor, bimap)

-- maybeToLeft :: Maybe a -> Either a b

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f
