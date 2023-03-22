import MinimizeTest qualified
import RegexTest qualified
import Utf8RangeTest qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "main"
      [
        Utf8RangeTest.tests,
        MinimizeTest.tests,
        RegexTest.tests
      ]
