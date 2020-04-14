module Regularity.Automata
where

import Regularity.Regex

import Test.QuickCheck

import qualified Data.Map as Map
import Data.Map (Map, (!))

import qualified Data.Set as Set
import Data.Set (Set)

import qualified Data.Text as T

{- PLAN

   - convert regex to automata
   - matching via automata
   - SPEED TEST LET'S GO!!!11!!!!11111

   - validity testing of automata
   - real show instance
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
  deriving Show

data Automaton =
  Automaton { states :: Map StateId State
{- TODO fold State into the Automaton
            , delta :: Map StateId (Map (Maybe Char) (Set StateId))
            , accepting :: Set StateId
-}
            , startState :: StateId
            }
  deriving Show

shiftBy :: StateId -> Int -> StateId
shiftBy (StateId i) n = StateId $ i + n

shiftStateBy :: State -> Int -> State
shiftStateBy st n =
  State { stateId = stateId st `shiftBy` n
        , transitions = Map.map (Set.map (`shiftBy` n)) (transitions st)
        , accepting = accepting st
        }

shiftAutomatonBy :: Automaton -> Int -> Automaton
shiftAutomatonBy a n =
  Automaton { states = Map.foldrWithKey
              (\si st states' ->
                 Map.insert (si `shiftBy` n) (st `shiftStateBy` n) states')
              Map.empty (states a)
            , startState = startState a `shiftBy` n
            }

maxStateId :: Automaton -> Int
maxStateId a = getStateId $ maximum $ Map.keys $ states a

stateOf :: StateId -> Automaton -> State
stateOf si a = states a ! si

accepts :: Automaton -> T.Text -> Bool
accepts a s = any (\si -> accepting (si `stateOf` a)) $ run a s

run :: Automaton -> T.Text -> Set StateId
run a = runIn (startState a)
  where
    runIn :: StateId -> T.Text -> Set StateId
    runIn si s =
      let currentStates = Set.insert si $ epsilonSteps a si
      in case T.uncons s of
           Nothing -> currentStates
           Just (c, s') ->
             let next = step a currentStates c in
             Set.foldr (\si' sts -> Set.union (runIn si' s') sts) Set.empty next

step :: Automaton -> Set StateId -> Char -> Set StateId
step a currentStates c =
  let nextStatesFor si = Map.findWithDefault Set.empty (Just c)
                         (transitions (si `stateOf` a))
      nextStates =
        Set.foldr (\si next -> Set.union (nextStatesFor si) next)
        Set.empty currentStates
  in
    Set.foldr (\si acc -> Set.union (epsilonSteps a si) acc) nextStates nextStates

-- BUG: potential infinite loop?  
epsilonSteps :: Automaton -> StateId -> Set StateId
epsilonSteps a initial = dfs [initial] Set.empty
  where
    dfs :: [StateId] -> Set StateId -> Set StateId
    dfs []       seen = seen
    dfs (si:sis) seen =
      let delta = transitions (si `stateOf` a)
          next  = Map.findWithDefault Set.empty Nothing delta
          new   = Set.filter (\si' -> not (si' `Set.member` seen)) next
      in
        dfs (Set.toList new ++ sis) (new `Set.union` seen)
                
empty :: Automaton
empty =
  let id0 = StateId 0
      s0  = State { stateId = id0
                  , transitions = Map.empty -- reject on no-match
                  , accepting = False
                  }
  in
  Automaton { states = Map.singleton id0 s0
            , startState = id0
            }

epsilon :: Automaton
epsilon =
  let id0 = StateId 0
      s0  = State { stateId = id0
                  , transitions = Map.empty
                  , accepting = True
                  }
  in
    Automaton { states = Map.singleton id0 s0
              , startState = id0
              }

char :: Char -> Automaton
char c =
  let id0 = StateId 0
      id1 = StateId 1
      s0  = State { stateId = id0
                  , transitions = Map.singleton (Just c) (Set.singleton id1)
                  , accepting = False
                  }
      s1  = State { stateId = id1
                  , transitions = Map.empty -- reject on no-match
                  , accepting = True
                  }
  in
    Automaton { states = Map.fromList [(id0, s0), (id1, s1)]
              , startState = id0
              }

seq :: Automaton -> Automaton -> Automaton
seq a1 a2 =
    let a2' = a2 `shiftAutomatonBy` (maxStateId a1 + 1)
        -- find every state in a1 that's accepting
        (a1Accepting, a1Rejecting) = Map.partition accepting $ states a1
        -- add an epsilon transtion to a2's start state
        a1AcceptingToA2 = Map.map
                   (\st -> st { transitions =
                                Map.insertWith Set.union Nothing
                                  (Set.singleton $ startState a2') $
                                  transitions st
                              , accepting = False })
                   a1Accepting
        a1' = Automaton { states = a1AcceptingToA2 `Map.union` a1Rejecting
                        , startState = startState a1
                        }
    in
      --       keep only a2's accepting states
      Automaton { states = states a1' `Map.union` states a2'
                , startState = startState a1'
                }

alt :: Automaton -> Automaton -> Automaton
alt a1 a2 =
  let a1' = a1 `shiftAutomatonBy` 1
      a2' = a2 `shiftAutomatonBy` (maxStateId a1 + 2) -- TODO + 1?
      id0 = StateId 0
      s0  = State { stateId = id0
                  , transitions = Map.singleton Nothing
                                  (Set.fromList [ startState a1'
                                                , startState a2'
                                                ])
                  , accepting = False
                  }
  in
    Automaton { states = Map.insert id0 s0 (states a1' `Map.union` states a2')
              , startState = id0
              }                                                        

star :: Automaton -> Automaton
star a =
  let
    (aAccepting, aRejecting) = Map.partition accepting $ states a
    aAcceptingToStart = Map.map
                        (\st -> st { transitions =
                                       Map.insertWith Set.union Nothing
                                       (Set.singleton $ startState a) $
                                       transitions st
                                   })
                        aAccepting
    sa = startState a
  in
    Automaton { states = Map.adjust (\st -> st { accepting = True }) sa
                         (aAcceptingToStart `Map.union` aRejecting)
              , startState = sa
              }
    
fromRegex :: Regex -> Automaton
fromRegex Empty         = empty
fromRegex Epsilon       = epsilon
fromRegex (Char c)      = char c
fromRegex (Alt re1 re2) = alt (fromRegex re1) (fromRegex re2)
fromRegex (Seq re1 re2) = Regularity.Automata.seq (fromRegex re1) (fromRegex re2)
fromRegex (Star re)     = star (fromRegex re)

instance Arbitrary Automaton where
  arbitrary = oneof [ pure empty
                    , pure epsilon
                    , char <$> arbitrary
                    ] -- TODO use fromRegex instead!
