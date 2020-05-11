module Regularity.Regular
  ( Regular(..)
  , fromRegex
  )
where

import Prelude hiding (seq)

import Regularity.Regex (Regex(..))
import qualified Regularity.Regex as R

class Regular a where
  empty, epsilon :: a
  char :: Char -> a
  seq :: a -> a -> a
  alt :: a -> a -> a
  star :: a -> a

fromRegex :: Regular a => Regex -> a
fromRegex Empty         = empty
fromRegex Epsilon       = epsilon
fromRegex (Char c)      = char c
fromRegex (Alt re1 re2) = alt (fromRegex re1) (fromRegex re2)
fromRegex (Seq re1 re2) = seq (fromRegex re1) (fromRegex re2)
fromRegex (Star re)     = star (fromRegex re)

instance Regular R.Regex where
  empty = R.empty
  epsilon = R.epsilon
  char = R.char
  alt = R.alt
  seq = R.seq
  star = R.star

  
