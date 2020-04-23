{-# LANGUAGE OverloadedStrings, BangPatterns #-}

-- | Regular expressions
module Regularity.Regex
  ( Regex(..)
  , Parser
  , matches
  , allMatches
  , size
  , starNesting
  , parse
  , forceLeftAssociation
  , textMatching
  , stringMatching
  )
where

import Text.Megaparsec hiding (parse)

import Test.QuickCheck

import qualified Data.Text as T
import Data.Void (Void)

import qualified Data.Set as Set
import Data.Set (Set)

-- | Regular expressions
-- TODO: make it generic over different characters
data Regex =
    Empty
  | Epsilon
  | Char Char
  | Seq !Regex !Regex
  | Alt !Regex !Regex
  | Star !Regex
  deriving (Eq, Ord)

-- | Regex matching

matches :: Regex -> T.Text -> Bool
matches !re !s = T.empty `elem` allMatches re s

-- | Returns the set of strings that could be remaining after a match.
allMatches :: Regex -> T.Text -> Set T.Text
allMatches Empty _ = Set.empty
allMatches Epsilon s = Set.singleton s
allMatches (Char c) s =
  case T.uncons s of
    Nothing -> Set.empty
    Just (c', s') -> if c == c' then Set.singleton s' else Set.empty
allMatches (Seq !re1 !re2) s =
  let remainder = allMatches re1 s in
  Set.foldr
    (\s' m -> Set.union (allMatches re2 s') m)
    Set.empty remainder
allMatches (Alt !re1 !re2) s =
  allMatches re1 s `Set.union` allMatches re2 s
allMatches (Star !re) s =
  let inners         = allMatches re s
      nonEmptyInners = Set.filter (not . (==s)) inners
  in
    Set.unions [ Set.singleton s
               , inners
               , Set.foldr
                   (\s' m -> Set.union (allMatches (Star re) s') m)
                   Set.empty nonEmptyInners
               ]

size :: Regex -> Int
size Empty         = 0
size Epsilon       = 1
size (Char _)      = 1
size (Seq re1 re2) = size re1 + size re2 + 1
size (Alt re1 re2) =  size re1 + size re2 + 1
size (Star re)     = size re + 1

starNesting :: Regex -> Int
starNesting Empty         = 0
starNesting Epsilon       = 0
starNesting (Char _)      = 0
starNesting (Seq re1 re2) = max (starNesting re1) (starNesting re2)
starNesting (Alt re1 re2) = max (starNesting re1) (starNesting re2)
starNesting (Star re)     = 1 + starNesting re

-- | Show instance

instance Show Regex where
  show = showAlt
    where
    showAlt (Alt re1 re2) = showAlt re1 ++ "|" ++ showAlt re2
    showAlt re            = showSeq re

    showSeq (Seq re1 re2) = showSeq re1 ++ showSeq re2
    showSeq re            = showStar re

    showStar (Star re)    = showAtom re ++ "*"
    showStar re           = showAtom re

    showAtom (Char c)     = backslash c ++ [c]
    showAtom Epsilon      = "𝜖"
    showAtom Empty        = "∅"
    showAtom re           = "(" ++ showAlt re ++ ")"

    backslash c | c `elem` specialChars = "\\"
                | otherwise             = ""

-- | Parsing

type Parser = Parsec Void T.Text

specialChars :: String
specialChars = "()|*𝜖∅\\"

parse, parseAlt, parseSeq, parseStar, parseAtom :: Parser Regex
parse = parseAlt

-- TODO: use the the expression parser generator
-- TODO: use lexing properly (spaces, etc.)
parseAlt = foldl1 Alt <$> sepBy1 parseSeq (single '|')
parseSeq = foldl1 Seq <$> some parseStar
parseStar =
      try (Star <$> parseAtom <* single '*')
  <|> parseAtom

parseAtom =
      try (Char <$> satisfy (not . (`elem` specialChars)))
  <|> try (Char <$> (single '\\' *> satisfy (`elem` specialChars)))
      -- TODO: support \n, \t, etc.
  <|> try (pure Epsilon <* single '𝜖')
  <|> try (pure Empty <* single '∅')
  <|> parens parseAlt

parens :: Parser a -> Parser a
parens = between (single '(') (single ')')

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
                      , Star <$> gen (n `div` 10)
                      ]

textMatching :: Regex -> Gen T.Text
textMatching re = T.pack <$> stringMatching re

stringMatching :: Regex -> Gen String
stringMatching Empty         = discard
stringMatching Epsilon       = pure ""
stringMatching (Char c)      = pure [c]
stringMatching (Seq re1 re2) = (++) <$> stringMatching re1 <*> stringMatching re2
stringMatching (Alt re1 re2) = oneof [stringMatching re1, stringMatching re2]
stringMatching (Star re)     = do
  n <- choose (0,5)
  concat <$> vectorOf n (scale (`div` n) $ stringMatching re)

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
