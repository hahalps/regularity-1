{-# LANGUAGE OverloadedStrings #-}

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Regularity hiding (main)

import Text.Megaparsec (parseMaybe)

import qualified Data.Text as T

main :: IO ()
main = do
  unitTests <- testSpecs unitTestSpec
  Test.Tasty.defaultMain $
    Test.Tasty.testGroup "regularity tests"
    [ Test.Tasty.testGroup "unit tests"
      unitTests
    , testProperties "property tests" quickCheckTests
    ]

unitTestSpec :: Spec
unitTestSpec = parallel $ do
    it "pretty printing 'abc'" $ do
      show (Seq (Seq (Char 'a') (Char 'b')) (Char 'c')) `shouldBe` "abc"

    it "pretty printing 'ab|c'" $ do
      show (Alt (Seq (Char 'a') (Char 'b')) (Char 'c')) `shouldBe` "ab|c"

    it "parsing 'ab|c'" $ do
      parseMaybe Regularity.parse "ab|c" `shouldBe`
        Just (Alt (Seq (Char 'a') (Char 'b')) (Char 'c'))

quickCheckTests :: [(String, Property)]
quickCheckTests =
  [ ("printing -> parsing round trip",
      forAll regexes $ \r -> parseMaybe Regularity.parse (T.pack (show r)) === Just r)
  ]


-- | Generators

instance Arbitrary Regex where
  arbitrary = regexes
  -- TODO implement shrinking

regexes :: Gen Regex
regexes = sized $ gen
  where gen 0 = oneof [ pure Empty
                      , pure Epsilon
                      , Char <$> arbitrary
                      ]
        gen n = oneof [ pure Empty
                      , pure Epsilon
                      , Char <$> arbitrary
                      , Seq <$> gen (n `div` 2) <*> gen (n `div` 2)
                      , Alt <$> gen (n `div` 2) <*> gen (n `div` 2)
                      , Star <$> gen (n `div` 2)
                      ]
