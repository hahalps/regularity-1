module Regularity.Automata
  ( StateId(..)
  , shiftStateIdBy
  , Automaton(..)
  )
where

import Prelude hiding (seq)

import Data.Text (Text)

import Regularity.Regex (Regex(..))

newtype StateId = StateId { getStateId :: Int }
  deriving (Eq, Ord)

instance Show StateId where
  show (StateId i) = "s_" ++ show i

shiftStateIdBy :: StateId -> Int -> StateId
shiftStateIdBy (StateId i) n = StateId $ i + n

class Automaton a where
  accepts :: a -> Text -> Bool

  aempty :: a
  aepsilon :: a
  achar :: Char -> a
  aseq :: a -> a -> a
  aalt :: a -> a -> a
  astar :: a -> a

  fromRegex :: Regex -> a
  fromRegex Empty         = aempty
  fromRegex Epsilon       = aepsilon
  fromRegex (Char c)      = achar c
  fromRegex (Alt re1 re2) = aalt (fromRegex re1) (fromRegex re2)
  fromRegex (Seq re1 re2) = aseq (fromRegex re1) (fromRegex re2)
  fromRegex (Star re)     = astar (fromRegex re)
