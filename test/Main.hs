import MinimizeTest qualified
import Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "main" [MinimizeTest.tests]
