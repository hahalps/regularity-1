{-# LANGUAGE OverloadedStrings, BangPatterns, TypeSynonymInstances, MultiParamTypeClasses #-}
module Regularity.Automata.NFA
  ( NFA()
  , shiftBy
  , fromNFAe
  )
where

import Prelude hiding (seq, init)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (intercalate, elemIndex)

import Regularity.Regular
import Regularity.Automata.Automaton

import Regularity.Automata.NFAe (NFAe)
import qualified Regularity.Automata.NFAe as NFAe

type StateId = Int

type StateMap a = IntMap a

shiftStateIdBy :: StateId -> Int -> StateId
shiftStateIdBy = (+)

type StateSet = IntSet

data NFA =
  NFA { states :: !StateSet
      , delta :: !(StateMap (Map Char StateSet))
      , acceptingStates :: !StateSet
      , startState :: !StateId
      }

instance Regular NFA where
  empty   = Regularity.Automata.NFA.empty
  epsilon = Regularity.Automata.NFA.epsilon
  char    = Regularity.Automata.NFA.char
  seq     = Regularity.Automata.NFA.seq
  alt     = Regularity.Automata.NFA.alt
  star    = Regularity.Automata.NFA.star

instance Automaton NFA StateSet where
  initialState a =
    IntSet.singleton $ startState a

  step a currentStates c =
    let nextStatesFor si =
          Map.findWithDefault IntSet.empty
            c (a `transitionsFor` si)
    in
      IntSet.foldr
        (\si next ->
           IntSet.union (nextStatesFor si) next)
        IntSet.empty
        currentStates

  accepting a ss =
    IntSet.foldr
      (\si found ->
         found || (si `IntSet.member` acceptingStates a))
      False
      ss

transitionsFor :: NFA -> StateId -> Map Char StateSet
transitionsFor a si = IntMap.findWithDefault Map.empty si (delta a)

instance Show NFA where
  show a =
    unlines $ [ "STATES: " ++ intercalate ", " (map show $ IntSet.toAscList $ states a)
              , "START STATE: " ++ (show $ startState a)
              , "ACCEPTING: " ++ intercalate ", " (map show $ IntSet.toAscList $ acceptingStates a)
              , "TRANSITIONS:"
              ] ++
    IntMap.foldrWithKey
    (\si trans ls ->
       let prefix = show si ++ ": "
           spaces = replicate (length prefix) ' '
       in
         prefix :
         Map.foldrWithKey
         (\c sis ls' ->
             let input = ['\'', c, '\'']
             in
               (spaces ++ input ++ " |-> " ++ intercalate ", " (map show $ IntSet.toAscList $ sis)) :
               ls'
             )
         ls
         trans)
    []
    (delta a)

shiftBy :: NFA -> Int -> NFA
shiftBy a n =
  NFA { states = IntSet.map (`shiftStateIdBy` n) (states a)
      , delta = IntMap.foldrWithKey
                (\si trans delta' ->
                    IntMap.insert (si `shiftStateIdBy` n)
                      (Map.map (IntSet.map (`shiftStateIdBy` n)) trans) delta')
                IntMap.empty
                (delta a)
      , startState = startState a `shiftStateIdBy` n
      , acceptingStates = IntSet.map (`shiftStateIdBy` n) (acceptingStates a)
      }

maxStateId :: NFA -> Int
maxStateId a = IntSet.findMax $ states a

empty :: NFA
empty =
  let id0 = 0
  in
  NFA { states = IntSet.singleton id0
      , delta = IntMap.singleton id0 Map.empty
      , startState = id0
      , acceptingStates = IntSet.empty
      }

epsilon :: NFA
epsilon =
  let id0 = 0
  in
    NFA { states = IntSet.singleton id0
        , delta = IntMap.singleton id0 Map.empty
        , startState = id0
        , acceptingStates = IntSet.singleton id0
        }

char :: Char -> NFA
char c =
  let id0 = 0
      id1 = 1
  in
    NFA { states = IntSet.fromList [id0, id1]
        , delta = IntMap.singleton id0 (Map.singleton c (IntSet.singleton id1))
        , startState = id0
        , acceptingStates = IntSet.singleton id1
        }

seq :: NFA -> NFA -> NFA
seq !a1 !a2 =
  -- CORE IDEA: acceptingStates states in a1 simulate a2's start state
  let a2' = a2 `shiftBy` (maxStateId a1 + 1)
      a2Inits = a2' `transitionsFor` startState a2'      
      delta1' = IntSet.foldr
                  (\si delta' ->
                     IntMap.insertWith (Map.unionWith IntSet.union)
                       si a2Inits delta')
                  (delta a1)
                  (acceptingStates a1)
  in
    NFA { states = states a1 `IntSet.union` states a2'
        , delta = delta1' `IntMap.union` delta a2'
        , startState = startState a1
        , acceptingStates =
            {- make sure that a1's accepting states
               are accepting if a2's startState is accepting
             -}
            (if startState a2' `IntSet.member` acceptingStates a2'
                 then acceptingStates a1
                 else IntSet.empty)
                `IntSet.union`
                acceptingStates a2'
              }

alt :: NFA -> NFA -> NFA
alt !a1 !a2 =
  let a1' = a1 `shiftBy` 1
      a2' = a2 `shiftBy` (maxStateId a1' + 1)
      id0 = 0
      a1Init = a1' `transitionsFor` startState a1'
      a2Init = a2' `transitionsFor` startState a2'
      init   = Map.unionWith IntSet.union a1Init a2Init
  in
    NFA { states = IntSet.insert id0 (states a1' `IntSet.union` states a2')
        , delta = IntMap.insert id0 init (delta a1' `IntMap.union` delta a2')
        , startState = id0
        , acceptingStates =
            IntSet.unions
            [ if startState a1' `IntSet.member` acceptingStates a1' || startState a2' `IntSet.member` acceptingStates a2'
              then IntSet.singleton id0
              else IntSet.empty
            , acceptingStates a1'
            , acceptingStates a2'
            ]
        }

star :: NFA -> NFA
star !a =
  let
    init   = a `transitionsFor` startState a
    delta' = IntSet.foldr
               (\si delta'' ->
                  IntMap.insertWith (Map.unionWith IntSet.union)
                    si init delta'')
               (delta a)
               (acceptingStates a)
  in
    NFA { states = states a
        , delta = delta'
        , startState = startState a
        , acceptingStates = IntSet.insert (startState a) (acceptingStates a)
        }

-- TODO fromNFAe

type LiftedState = StateSet

fromNFAe :: NFAe -> NFA
fromNFAe ae =
  {- states in ae are run by taking epsilonSteps after them

     we want an automaton that doesn't have any epsilon steps

     CORE IDEA: convert sets of ae states to single states, where
                we form equivalence classes of states that epsilon step to each other
   -}
  let
    -- LIFTED to sets of states
    lStartState :: LiftedState
    lStartState = NFAe.epsilonSteps ae
                  (IntSet.singleton (NFAe.startState ae))

    lTransitionsFor :: LiftedState -> Map Char (Set LiftedState)
    lTransitionsFor ls =
      Map.unionsWith Set.union $
      map (Map.foldrWithKey
             (\mc ls' tr ->
                case mc of
                  Nothing -> tr
                  Just c -> Map.insertWith Set.union c (Set.singleton (NFAe.epsilonSteps ae ls')) tr)
             Map.empty .
           NFAe.transitionsFor ae) $
      IntSet.toList ls

    dfs :: [LiftedState] -> Set LiftedState ->
           Map LiftedState (Map Char (Set LiftedState)) -> (Map LiftedState (Map Char (Set LiftedState)), Set LiftedState)
    dfs []      seen tr = (tr, seen)
    dfs (ls:wl) seen tr =
      {- WL = [lStartState]
         while WL <> []:
           ls = pop(WL)
           for (c,lNext_c) in delta(ls):
             add c |-> epsilonSteps lNext_c in our new delta for ls
             add epsilonSteps lNext_c to our WL if not seen before
       -}
      let ltr = lTransitionsFor ls :: Map Char (Set LiftedState)

          tr' = Map.unionWith (Map.unionWith Set.union) (Map.singleton ls ltr) tr

          seen' = Set.insert ls seen                 
          next  = Set.unions $ Map.elems ltr
          wl'   = Set.toList (next `Set.difference` seen) ++ wl
      in
        dfs wl' seen' tr'
    
    (lDelta, lStates) = dfs [lStartState] (Set.singleton lStartState) Map.empty

    -- state map
    stateMap = Set.toList lStates
    stateIdFor :: StateSet -> StateId
    stateIdFor ls =
      case elemIndex ls stateMap of
        Nothing ->
          error ("bad state set, not in set of states: " ++ show ls)
        Just idx -> idx
  in
    NFA { states = Set.foldr (\ls ss -> IntSet.insert (stateIdFor ls) ss) IntSet.empty lStates
        , startState = stateIdFor lStartState
        , delta = Map.foldrWithKey
                    (\ls tr delta' ->
                       IntMap.insert
                         (stateIdFor ls)
                         (Map.map (Set.foldr (\ls' ss -> IntSet.insert (stateIdFor ls') ss) IntSet.empty) tr)
                         delta')
                    IntMap.empty
                    lDelta
        , acceptingStates = Set.foldr
                        (\ls acc ->
                           if IntSet.foldr (\sid found -> found || sid `IntSet.member` NFAe.acceptingStates ae) False ls
                           then IntSet.insert (stateIdFor ls) acc
                           else acc)
                        IntSet.empty
                        lStates
        }
