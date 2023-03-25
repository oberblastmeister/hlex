module MinimizeTest (tests) where

import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import GHC.Exts (fromList)
import Test.Tasty
import Test.Tasty.HUnit
import Ilex.Internal.Dfa
import Ilex.Internal.Minimize qualified as Minimize

newState' :: Int -> s -> State' s a
newState' k = newState . IntMap.singleton k

tests :: TestTree
tests =
  testGroup
    "Minimize"
    [
      -- testCase "smoke" $ do
      --   let dfa =
      --         normalize $
      --           Dfa
      --             { start = 0 :: Int,
      --               states =
      --                 HashMap.fromList
      --                   [ (0, newState' 1 1),
      --                     (1, newState $ fromList [(2, 2), (3, 3)]),
      --                     (2, newState' 4 4),
      --                     (3, newState' 4 5),
      --                     (4, newState' 6 6),
      --                     (5, newState' 6 6),
      --                     (6, State mempty (Just ()))
      --                   ]
      --             }
      --       equiv = Minimize.dfaEquivalentStates dfa
      --       dfa' = Minimize.minimize dfa
      --   -- print dfa
      --   -- print equiv
      --   -- print dfa'
      --   pure ()
    ]
