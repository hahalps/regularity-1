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
import qualified Regularity.Regex as Regex
import Regularity.Regex (Regex(..), matches)

import qualified Regularity.Automata as Automata

import Text.Megaparsec (parseMaybe)

import qualified Data.Text as T

main :: IO ()
main = do
  unitTests <- testSpecs unitTestSpec
  Test.Tasty.defaultMain $
    Test.Tasty.testGroup "regularity tests"
    [ Test.Tasty.testGroup "unit tests"
      unitTests
    , testProperties "regex property tests" regexPropertyTests
    , testProperties "automata property tests" automataPropertyTests
    ]

unitTestSpec :: Spec
unitTestSpec = parallel $ do
    it "pretty printing 'abc'" $ do
      show (Seq (Seq (Char 'a') (Char 'b')) (Char 'c')) `shouldBe` "abc"

    it "pretty printing 'ab|c'" $ do
      show (Alt (Seq (Char 'a') (Char 'b')) (Char 'c')) `shouldBe` "ab|c"

    it "parsing 'ab|c'" $ do
      parseMaybe Regex.parse "ab|c" `shouldBe`
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

regexPropertyTests :: [(String, Property)]
regexPropertyTests =
  [ ("printing -> parsing round trip up to left association"
    , property $ \r -> parseMaybe Regex.parse (T.pack (show r)) ===
                       Just (Regex.forceLeftAssociation r))
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
    , property $ \r s -> not $ matches (Seq r Empty) (T.pack s)) -- forcing small sizes so that we don't spend forever in a slow matching routine
{-  , ("random regex sizes"
    , property $ \r ->
        forAll (textMatching r) $ \s ->
        label ("for a regex of size " ++ show (size r) ++ " with " ++ show (starNesting r ) ++ " nested stars, found a string of length " ++ show (T.length s))
        True
    ) -}
  , ("stringMatching correct"
    , property $ \r -> forAll (textMatching r) $ \s -> property (r `matches` s)
    )
        
  ]

automataPropertyTests :: [(String, Property)]
automataPropertyTests =
  [ ("empty automaton rejects all strings"
    , property $ \s -> not (Automata.empty `Automata.accepts` T.pack s))
  , ("epsilon automaton accepts only the empty string"
    , property $ \rawS ->
        let s = T.pack rawS
            isEmpty = T.null s
        in
          classify isEmpty "empty" $
          isEmpty === Automata.epsilon `Automata.accepts` s)
  , ("char automaton accepts only its character"
    , property $ \c rawS ->
        let a = Automata.char c
            s = T.pack rawS
            
            startsWithC = case T.uncons s of
                            Nothing -> False
                            Just (c',_) -> c == c'

            accepting = a `Automata.accepts` s
        in
          classify startsWithC "already start with c" $
          classify accepting "accepting" $
          (startsWithC && T.length s == 1) === accepting)
  , ("char automaton accepts only its character (forcing char on front)"
    , property $ \c rawS ->
        let a = Automata.char c
            s = T.pack (c : rawS)

            accepting = a `Automata.accepts` s
        in
          classify accepting "accepting" $
          (T.length s == 1) === accepting)
  , ("char automaton accepts strings matching exactly"
    , property $ \c ->
        Automata.char c `Automata.accepts` T.singleton c)
  , ("alt automaton accepts either character"
    , property $ \c1 c2 ->
        let a = Automata.alt (Automata.char c1) (Automata.char c2) in
          conjoin [ a `Automata.accepts` T.singleton c1
                  , a `Automata.accepts` T.singleton c2
                  , not (a `Automata.accepts` T.empty)
                  , not (a `Automata.accepts` T.pack [c1,c2])
                  ])
  , ("seq automaton accepts characters in order"
    , property $ \c1 c2 ->
        let a = Automata.seq (Automata.char c1) (Automata.char c2) in
          conjoin [ a `Automata.accepts` T.pack [c1, c2]
                  , not (a `Automata.accepts` T.empty)
                  , not (a `Automata.accepts` T.pack [c1])
                  , not (a `Automata.accepts` T.pack [c2])
                  , c1 == c2 || not (a `Automata.accepts` T.pack [c1,c1])
                  , c1 == c2 || not (a `Automata.accepts` T.pack [c2,c2])
                  ])
  , ("star automaton accepts empty string"
    , property $ \c -> Automata.star (Automata.char c) `Automata.accepts` T.empty)

  , ("star automaton accepts single char"
    , property $ \c -> Automata.star (Automata.char c) `Automata.accepts` T.singleton c)

  , ("star automaton accepts ten characters"
    , property $ \c ->
        Automata.star (Automata.char c) `Automata.accepts` T.replicate 10 (T.singleton c))

  , ("star automaton doesn't accept weird suffixes"
    , property $ \c ->
        not (Automata.star (Automata.char c) `Automata.accepts` T.pack [c,c,c,c,c, 'b', 'c']))

  , ("regexes and automata agree"
    , property $ \re rawS ->
        let s = T.pack rawS
            accepting = re `matches` s
        in
          classify accepting "accepting" $
          classify (T.null s) "empty string" $
          accepting === (Automata.fromRegex re `Automata.accepts` s))

  , ("regexes and automata agree on accepting states"
    , noShrinking $ property $ \re -> forAll (textMatching re) $ \s ->
        classify (T.null s) "empty string" $
        collect (show (starNesting re) ++ " nested stars") $
        re `matches` s .&&. (Automata.fromRegex re `Automata.accepts` s))

  , ("shifting ids doesn't change acceptance"
    , property $ \a rawS (Positive n)->
        let s = T.pack rawS
            accepting = a `Automata.accepts` s
        in
          classify accepting "accepting" $
          accepting === (a `Automata.shiftAutomatonBy` n) `Automata.accepts` s)
  ]
