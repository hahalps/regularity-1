{-# LANGUAGE OverloadedStrings #-}

-- | The top-level Regularity module.
module Regularity
{-  ( main
  , Regex(..)
  , Parser
  ) -}
where

{- next time:

   - define automata
   - convert regex to automata
   - matching via automata
   - SPEED TEST LET'S GO!!!11!!!!11111
-}

import Text.Megaparsec

import qualified Data.Text as T
import Data.Void (Void)

import qualified Data.Set as Set
import Data.Set (Set)

-- | Default entry point.
main :: IO ()
main =
  putStrLn "regularity: coming soon"

-- | Regular expressions
-- TODO: make it generic over different characters
data Regex =
    Empty
  | Epsilon
  | Char Char
  | Seq Regex Regex
  | Alt Regex Regex
  | Star Regex
  deriving (Eq, Ord)

-- | Regex matching

matches :: Regex -> T.Text -> Bool
matches re s = T.empty `elem` allMatches re s

allMatches :: Regex -> T.Text -> Set T.Text
allMatches Empty _ = Set.empty
allMatches Epsilon s = if T.null s then Set.singleton T.empty else Set.empty
allMatches (Char c) s =
  case T.uncons s of
    Nothing -> Set.empty
    Just (c', s') -> if c == c' then Set.singleton s' else Set.empty
allMatches (Seq re1 re2) s =
  Set.foldr
    (\s' m -> Set.union (allMatches re2 s') m)
    Set.empty (allMatches re1 s)
allMatches (Alt re1 re2) s =
  allMatches re1 s `Set.union` allMatches re2 s
allMatches (Star re) s =
  let inners         = allMatches re s
      nonEmptyInners = Set.filter (not . T.null) inners
  in
    Set.unions [ Set.singleton s
               , inners
               , Set.foldr
                   (\s' m -> Set.union (allMatches (Star re) s') m)
                   Set.empty nonEmptyInners
               ]

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
