module Regularity.Automata
  ( StateId(..)
  , shiftStateIdBy
  , Automaton(..)
  )
where

import Prelude hiding (seq)

import Data.Set (Set)
import qualified Data.Set as Set

import Data.Text (Text)
import qualified Data.Text as T

import Test.QuickCheck

import Regularity.Regex (Regex(..))

newtype StateId = StateId { getStateId :: Int }
  deriving (Eq, Ord)

instance Show StateId where
  show (StateId i) = "s_" ++ show i

shiftStateIdBy :: StateId -> Int -> StateId
shiftStateIdBy (StateId i) n = StateId $ i + n

class Automaton a where
  accepts :: a -> Text -> Bool

  empty :: a
  epsilon :: a
  char :: Char -> a
  seq :: a -> a -> a
  alt :: a -> a -> a
  star :: a -> a

  fromRegex :: Regex -> a
  fromRegex Empty         = empty
  fromRegex Epsilon       = epsilon
  fromRegex (Char c)      = char c
  fromRegex (Alt re1 re2) = alt (fromRegex re1) (fromRegex re2)
  fromRegex (Seq re1 re2) = seq (fromRegex re1) (fromRegex re2)
  fromRegex (Star re)     = star (fromRegex re)
