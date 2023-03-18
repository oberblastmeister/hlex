module Text.HLex.Internal.AssocList
  ( AssocList (..),
    fromList,
    toList,
    toHashMap,
  )
where

import Data.Bifunctor (Bifunctor (bimap))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import GHC.Exts (IsList (Item))
import GHC.Exts qualified
import GHC.Generics (Generic)

newtype AssocList k v = AssocList {unAssocList :: [(k, v)]}
  deriving (Generic)
  deriving (Show, Eq, Ord) via [(k, v)]
  deriving (Functor, Foldable, Traversable)

instance Bifunctor AssocList where
  bimap f g = AssocList . fmap (bimap f g) . unAssocList

instance IsList (AssocList k v) where
  type Item (AssocList k v) = (k, v)
  fromList = Text.HLex.Internal.AssocList.fromList
  toList = Text.HLex.Internal.AssocList.toList

fromList :: [(k, v)] -> AssocList k v
fromList = AssocList

toList :: AssocList k v -> [(k, v)]
toList = unAssocList

toHashMap :: Hashable k => AssocList k v -> HashMap k v
toHashMap = HashMap.fromList . toList
