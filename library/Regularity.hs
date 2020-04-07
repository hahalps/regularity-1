-- | The top-level Regularity module.
module Regularity
  ( main
  , Regex(..)
  )
where

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
    showAlt (Alt re1 re2) = showAlt re1 ++ " | " ++ showAlt re2
    showAlt re            = showSeq re

    showSeq (Seq re1 re2) = showSeq re1 ++ showSeq re2
    showSeq re            = showStar re

    showStar (Star re)    = showAtom re ++ "*"
    showStar re           = showAtom re

    showAtom (Char c)     = [c]
    showAtom Epsilon      = "𝜖"
    showAtom Empty        = "∅"
    showAtom re           = "(" ++ showAlt re ++ ")"
