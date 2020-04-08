{-# LANGUAGE OverloadedStrings #-}

-- | The top-level Regularity module.
module Regularity
{-  ( main
  , Regex(..)
  , Parser
  ) -}
where

import Text.Megaparsec

import Data.Text hiding (foldl, foldl1)
import Data.Void (Void)

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

instance Show Regex where
  show = showAlt
    where
    showAlt (Alt re1 re2) = showAlt re1 ++ "|" ++ showSeq re2
    showAlt re            = showSeq re

    showSeq (Seq re1 re2) = showSeq re1 ++ showStar re2
    showSeq re            = showStar re

    showStar (Star re)    = showAtom re ++ "*"
    showStar re           = showAtom re

    showAtom (Char c)     = backslash c ++ [c]
    showAtom Epsilon      = "𝜖"
    showAtom Empty        = "∅"
    showAtom re           = "(" ++ showAlt re ++ ")"

    backslash c | c `elem` specialChars = "\\"
                | otherwise             = ""

type Parser = Parsec Void Text

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
