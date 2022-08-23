module MinimizeTest where

import Data.HashMap.Strict qualified as HashMap
import Data.IntMap.Strict qualified as IntMap
import Data.Vector.Persistent qualified as PVec
import GHC.Exts (fromList)
import Test.QuickCheck
import Text.HLex.Internal.Accept
import Text.HLex.Internal.Dfa
import Text.HLex.Internal.Minimize qualified as Minimize

-- instance Arbitrary a => Arbitrary (Dfa a) where
--   arbitrary = sized $ \n -> do
--     ss <- chooseInt (1, n)
--     let alphabet = [0 .. 10]
--     states <- forM_ ss $ \s -> do
--       to <- chooseInt (1, n)
--       accept <- arbitrary
--       pure (s, State {transitions = })

unit_smoke :: IO ()
unit_smoke = do
  let dfa :: Dfa String =
        normalize $
          Dfa
            { starts = [0 :: Int],
              states =
                HashMap.fromList
                  [ (0, newState' 1 1),
                    (1, newState $ fromList [(2, 2), (3, 3)]),
                    (2, newState' 4 4),
                    (3, newState' 4 5),
                    (4, State mempty (Just (Accept "first" 1))),
                    (5, State mempty (Just (Accept "second" 1)))
                  ]
            }
      equiv = Minimize.dfaEquivalentStates dfa
      dfa' = Minimize.minimize dfa
  print dfa
  print equiv
  -- print dfa'
  pure ()

newState' :: Int -> s -> State s a
newState' k = newState . IntMap.singleton k
