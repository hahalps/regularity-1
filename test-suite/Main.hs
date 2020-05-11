{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}

-- Tasty makes it easy to test your code. It is a test framework that can
-- combine many different types of tests into one suite. See its website for
-- help: <http://documentup.com/feuerbach/tasty>.
import qualified Test.Tasty
-- Hspec is one of the providers for Tasty. It provides a nice syntax for
-- writing tests. Its website has more info: <https://hspec.github.io>.
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Prelude hiding (seq)

import Regularity.Regex (Regex(..), matches, starNesting, textMatching, Alphabet, regexesOfSize)
import Regularity.Brzozowski (Brzozowski(..), deriv)
import qualified Regularity.Regex as Regex

import Regularity.Regular

import Regularity.Automata
import Regularity.Automata.NFAe (NFAe)
import qualified Regularity.Automata.NFAe as NFAe
import Regularity.Automata.NFA (NFA)
import qualified Regularity.Automata.NFA as NFA

import Text.Megaparsec (parseMaybe)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as T

main :: IO ()
main = do
  unitTests <- testSpecs unitTestSpec
  Test.Tasty.defaultMain $
    Test.Tasty.testGroup "regularity tests"
    [ Test.Tasty.testGroup "unit tests"
      unitTests
    , testProperties "regex property tests" regexPropertyTests
    , testProperties "automata property tests (NFAe)" (automataPropertyTests (accepts :: NFAe -> Text -> Bool))
    , testProperties "automata property tests (NFA)" (automataPropertyTests (accepts :: NFA -> Text -> Bool))
    , testProperties "automata property tests (implicit Brzozowski)" (automataPropertyTests (accepts :: Brzozowski -> Text -> Bool))
    , testProperties "automata id shifting (NFAe)" (automataShiftTests NFAe.shiftBy)
    , testProperties "automata id shifting (NFA)" (automataShiftTests NFA.shiftBy)
    , testProperties "epsilon determinization"
      [ ("arbitrary regexes (accepting only)",
          property $ \r -> forAll (textMatching r) $ \s ->
            let ae = fromRegex r :: NFAe
                a  = NFA.fromNFAe ae
            in
              ae `accepts` s .&&. a `accepts `s)
      , ("arbitrary regexes (arbitrary strings)"
        , property $ \re rawS ->
            let s  = T.pack rawS
                ae = fromRegex re :: NFAe
                a  = NFA.fromNFAe ae :: NFA
                accepting = ae `accepts` s
            in
              classify accepting "accepting" $
              classify (T.null s) "empty string" $
              accepting === (a `accepts` s))

      ]
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

    let sigma = Set.fromList "ab"
    regexesOfSizeCorrect sigma 0
    regexesOfSizeCorrect sigma 1
    regexesOfSizeCorrect sigma 2
    regexesOfSizeCorrect sigma 3
    regexesOfSizeCorrect sigma 4
    regexesOfSizeCorrect sigma 5

    let sigma = Set.fromList "abc"
    regexesOfSizeCorrect sigma 0
    regexesOfSizeCorrect sigma 1
    regexesOfSizeCorrect sigma 2
    regexesOfSizeCorrect sigma 3
    regexesOfSizeCorrect sigma 4
    regexesOfSizeCorrect sigma 5

regexesOfSizeCorrect :: Alphabet -> Int -> SpecWith ()
regexesOfSizeCorrect sigma n =
  it ("regexesOfSize " ++ show n ++ " correctly sized") $ do
    (all ((==n) . Regex.size) $ regexesOfSize sigma n)
      `shouldBe` True

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
  , ("textMatching correct"
    , property $ \r -> forAll (textMatching r) $ \s -> property (r `matches` s)
    )
  , ("nullable correct"
    , property $ \r -> Regex.nullable r === (r `matches` T.empty))
  , ("deriv correct (accepting only)"
    , property $ \r -> forAll (textMatching r) $ \s ->
        case T.uncons s of
          Nothing -> property Discard
          Just (c,s') -> property (deriv c r `matches` s'))
  , ("derivitive wrt fresh char is empty"
    , property $ \r ->        
        let sigma = Regex.alphabetOf r
        in
          not (Set.null sigma) ==>
          forAll arbitrary $ \c ->
            not (c `Set.member` sigma) ==>
            label ("|sigma| = " ++ show (Set.size sigma)) $
            deriv c r === Empty)
  ]

automataShiftTests :: (Regular a, Automaton a s) => (a -> Int -> a) -> [(String, Property)]
automataShiftTests shiftBy =
  [ ("shifting ids doesn't change acceptance"
    , property $ \re rawS (Positive n)->
        let s = T.pack rawS
            a = fromRegex re
            accepting = a `accepts` s
        in
          classify accepting "accepting" $
          accepting === (a `shiftBy` n) `accepts` s)
  , ("shifting ids doesn't change acceptance on known-good strings"
    , property $ \re (Positive n) -> forAll (textMatching re) $ \s ->
        let a = fromRegex re in
          (a `accepts` s) .&&. ((a `shiftBy` n) `accepts` s))
  ]

automataPropertyTests :: Regular a => (a -> Text -> Bool) -> [(String, Property)]
automataPropertyTests accepts =
  [ ("empty automaton rejects all strings"
    , property $ \s -> not (empty `accepts` T.pack s))
  , ("epsilon automaton accepts only the empty string"
    , property $ \rawS ->
        let s = T.pack rawS
            isEmpty = T.null s
        in
          classify isEmpty "empty" $
          isEmpty === epsilon `accepts` s)
  , ("char automaton accepts only its character"
    , property $ \c rawS ->
        let a = char c
            s = T.pack rawS
            
            startsWithC = case T.uncons s of
                            Nothing -> False
                            Just (c',_) -> c == c'

            accepting = a `accepts` s
        in
          classify startsWithC "already start with c" $
          classify accepting "accepting" $
          (startsWithC && T.length s == 1) === accepting)
  , ("char automaton accepts only its character (forcing char on front)"
    , property $ \c rawS ->
        let a = char c
            s = T.pack (c : rawS)

            accepting = a `accepts` s
        in
          classify accepting "accepting" $
          (T.length s == 1) === accepting)
  , ("char automaton accepts strings matching exactly"
    , property $ \c ->
        char c `accepts` T.singleton c)
  , ("alt automaton accepts either character"
    , property $ \c1 c2 ->
        let a = alt (char c1) (char c2) in
          conjoin [ a `accepts` T.singleton c1
                  , a `accepts` T.singleton c2
                  , not (a `accepts` T.empty)
                  , not (a `accepts` T.pack [c1,c2])
                  ])
  , ("seq automaton accepts characters in order"
    , property $ \c1 c2 ->
        let a = seq (char c1) (char c2) in
          conjoin [ a `accepts` T.pack [c1, c2]
                  , not (a `accepts` T.empty)
                  , not (a `accepts` T.pack [c1])
                  , not (a `accepts` T.pack [c2])
                  , c1 == c2 || not (a `accepts` T.pack [c1,c1])
                  , c1 == c2 || not (a `accepts` T.pack [c2,c2])
                  ])
  , ("star automaton accepts empty string"
    , property $ \c -> star (char c) `accepts` T.empty)

  , ("star automaton accepts single char"
    , property $ \c -> star (char c) `accepts` T.singleton c)

  , ("star automaton accepts ten characters"
    , property $ \c ->
        star (char c) `accepts` T.replicate 10 (T.singleton c))

  , ("star automaton doesn't accept weird suffixes"
    , property $ \c ->
        not (star (char c) `accepts` T.pack [c,c,c,c,c, 'b', 'c']))

  , ("correct on nullable regexes/empty string"
    , property $ \r -> Regex.nullable r === (fromRegex r `accepts` T.empty))
  
  , ("regexes and automata agree"
    , property $ \re rawS ->
        let s = T.pack rawS
            accepting = re `matches` s
        in
          classify accepting "accepting" $
          classify (T.null s) "empty string" $
          let a = fromRegex re in
          accepting === (a `accepts` s))
    
  , ("regexes and automata agree on accepting states"
    , noShrinking $ property $ \re -> forAll (textMatching re) $ \s ->
        classify (T.null s) "empty string" $
        collect (show (starNesting re) ++ " nested stars") $
        let a = fromRegex re in
        re `matches` s .&&. (a `accepts` s))
  ]
