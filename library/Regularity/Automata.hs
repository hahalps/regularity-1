module Regularity.Automata
where

import Regularity.Regex

import qualified Data.Map as Map
import Data.Map (Map, (!))

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Text as T

{- PLAN

   - convert regex to automata
   - matching via automata
   - SPEED TEST LET'S GO!!!11!!!!11111
-}

newtype StateId = StateId { getStateId :: Int }
  deriving (Eq, Ord)

instance Show StateId where
  show (StateId i) = "s_" ++ show i

-- epsilon transitions: Nothing is epsilon; Just c is a character

data State =
  State { stateId :: StateId
        , transitions :: Map (Maybe Char) (Set StateId)
        , accepting :: Bool
        }

data Automaton =
  Automaton { states :: Map StateId State
            , startState :: StateId
            }

stateOf :: StateId -> Automaton -> State
stateOf si a = states a ! si

accepts :: Automaton -> T.Text -> Bool
accepts a s = any (\si -> accepting (si `stateOf` a)) $ run a s

run :: Automaton -> T.Text -> Set StateId
run a = runIn (startState a)
  where
    runIn :: StateId -> T.Text -> Set StateId
    runIn si s =
          case T.uncons s of
            Nothing -> Set.singleton si
            Just (c, s') ->
              let delta = transitions (si `stateOf` a)
                  enext = Map.findWithDefault Set.empty Nothing delta
                  cnext = Map.findWithDefault Set.empty (Just c) delta
                  next  = enext `Set.union` cnext
              in
                Set.foldr (\si' sts -> Set.union (runIn si' s') sts) Set.empty next
                
empty :: Automaton
empty =
  let id0 = StateId 0
      s0  = State { stateId = id0
                  , transitions = Map.empty -- what happens??
                  , accepting = False
                  }
  in
  Automaton { states = Map.singleton id0 s0
            , startState = id0
            }
  
fromRegex :: Regex -> Automaton
fromRegex Empty = empty  
fromRegex re = error ("fromRegex: unimplemented on " ++ show re)

