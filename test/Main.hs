import MinimizeTest qualified
import RegexTest qualified
import Test.Tasty

main :: IO ()
main =
  defaultMain $
    testGroup
      "main"
      [ MinimizeTest.tests,
        RegexTest.tests
      ]
