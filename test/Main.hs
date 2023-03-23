import MinimizeTest qualified
import RegexTest qualified
import Utf8Test qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "main"
      [
        Utf8Test.tests,
        MinimizeTest.tests,
        RegexTest.tests
      ]
