-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec

import Regularity hiding (main)

main :: IO ()
main = do
    test <- testSpec "regularity" spec
    Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
    it "pretty printing 'abc'" $ do
      show (Seq (Seq (Char 'a') (Char 'b')) (Char 'c')) `shouldBe` "abc"

    it "pretty printing 'ab | c'" $ do
      show (Alt (Seq (Char 'a') (Char 'b')) (Char 'c')) `shouldBe` "ab | c"
