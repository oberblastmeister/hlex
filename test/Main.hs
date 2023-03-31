import ContextTest qualified
import LexerTest qualified
import LuaTest qualified
import MinimizeTest qualified
import RegexTest qualified
import SimpleTest qualified
import Test.Tasty
import Utf8Test qualified

main :: IO ()
main =
  defaultMain $
    testGroup
      "main"
      [ SimpleTest.tests,
        Utf8Test.tests,
        MinimizeTest.tests,
        RegexTest.tests,
        LexerTest.tests,
        ContextTest.tests,
        LuaTest.tests
      ]
