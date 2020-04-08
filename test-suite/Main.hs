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

    let re_aStar = Star (Char 'a')
    it "matching a* on a" $ do
      matches re_aStar "a" `shouldBe` True
    it "matching a* on aa" $ do
      matches re_aStar "aa" `shouldBe` True
    it "matching a* on aaa" $ do
      matches re_aStar "aaa" `shouldBe` True
    it "matching a* on aaaa" $ do
      matches re_aStar "aaaa" `shouldBe` True

    let re_epsAStar = Star (Alt Epsilon (Char 'a'))
    it "matching (𝜖|a)* on a" $ do
      matches re_epsAStar "a" `shouldBe` True
    it "matching (𝜖|a)* on aa" $ do
      matches re_epsAStar "aa" `shouldBe` True
    it "matching (𝜖|a)* on aaa" $ do
      matches re_epsAStar "aaa" `shouldBe` True
    it "matching (𝜖|a)* on aaaa" $ do
      matches re_epsAStar "aaaa" `shouldBe` True

quickCheckTests :: [(String, Property)]
quickCheckTests =
  [ ("printing -> parsing round trip up to left association"
    , property $ \r -> parseMaybe Regularity.parse (T.pack (show r)) === Just (forceLeftAssociation r))
  , ("star always matches empty string"
    , property $ \r -> (Star r) `matches` T.empty)
  , ("𝜖|... always matches empty string"
    , property $ \r -> (Alt Epsilon r) `matches` T.empty)
  , ("...|𝜖 always matches empty string"
    , property $ \r -> (Alt r Epsilon) `matches` T.empty)
  , ("∅ matches nothing"
    , property $ \s -> not $ matches Empty (T.pack s))
  , ("∅... matches nothing"
    , property $ \r s -> not $ matches (Seq Empty r) (T.pack s))
  , ("...∅ matches nothing"
    , mapSize (`div` 50) $ \r s -> not $ matches (Seq r Empty) (T.pack s)) -- forcing small sizes so that we don't spend forever in a slow matching routine
  ]

-- | Generators

instance Arbitrary Regex where
  arbitrary = regexes
  shrink = shrinkRegex

shrinkRegex :: Regex -> [Regex]
shrinkRegex Empty         = []
shrinkRegex Epsilon       = [Empty]
shrinkRegex (Char _c)     = map Char ['a'..'z'] ++ [Epsilon, Empty]
shrinkRegex (Seq re1 re2) = [Epsilon, Empty] ++
                            [re1, re2] ++
                            [Seq re1' re2' | re1' <- shrinkRegex re1, re2' <- shrinkRegex re2]
shrinkRegex (Alt re1 re2) = [Epsilon, Empty] ++
                            [re1, re2] ++
                            [Alt re1' re2' | re1' <- shrinkRegex re1, re2' <- shrinkRegex re2]
shrinkRegex (Star re)     = [Epsilon, Empty] ++
                            [re] ++
                            [Star re' | re' <- shrinkRegex re]

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

forceLeftAssociation :: Regex -> Regex
forceLeftAssociation (Seq re1 re2) =
  let lhs = collectSeqs re1
      rhs = collectSeqs re2
  in
    foldl1 Seq $ map forceLeftAssociation (lhs ++ rhs)
forceLeftAssociation (Alt re1 re2) =
  let lhs = collectAlts re1
      rhs = collectAlts re2
  in
    foldl1 Alt $ map forceLeftAssociation (lhs ++ rhs)
forceLeftAssociation Empty     = Empty
forceLeftAssociation Epsilon   = Epsilon
forceLeftAssociation (Char c)  = Char c
forceLeftAssociation (Star re) = Star $ forceLeftAssociation re

collectSeqs :: Regex -> [Regex]
collectSeqs (Seq re1 re2) = collectSeqs re1 ++ collectSeqs re2
collectSeqs re            = [re]

collectAlts :: Regex -> [Regex]
collectAlts (Alt re1 re2) = collectAlts re1 ++ collectAlts re2
collectAlts re            = [re]
