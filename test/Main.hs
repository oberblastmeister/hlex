import ContextTest qualified
import LexerTest qualified
import MinimizeTest qualified
import RegexTest qualified
import Test.Tasty
import Utf8Test qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "main"
      [ Utf8Test.tests,
        MinimizeTest.tests,
        RegexTest.tests,
        LexerTest.tests,
        ContextTest.tests
      ]
