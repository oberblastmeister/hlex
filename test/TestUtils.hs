module TestUtils where

import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as TL
import System.FilePath ((<.>), (</>))
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden qualified as Golden
import Text.Pretty.Simple (pShowNoColor)

pShow' :: Show a => a -> Text
pShow' = TL.toStrict . pShowNoColor

testGoldenShow :: Show a => String -> IO a -> Tasty.TestTree
testGoldenShow name action = testGolden name do pShow' <$> action

testGolden :: String -> IO Text -> Tasty.TestTree
testGolden name action = testGolden' name ("testdata" </> name <.> "golden") action

testGoldenInShow :: Show a => FilePath -> String -> IO a -> Tasty.TestTree
testGoldenInShow dir name action = testGolden' name ("testdata" </> dir </> name <.> "golden") do pShow' <$> action

testGoldenIn :: String -> FilePath -> IO Text -> Tasty.TestTree
testGoldenIn name dir action = testGolden' name (dir </> name <.> "golden") action

testGolden' :: String -> FilePath -> IO Text -> Tasty.TestTree
testGolden' name path action = Golden.goldenVsString name path do
  BL.fromStrict . T.encodeUtf8 <$> action
