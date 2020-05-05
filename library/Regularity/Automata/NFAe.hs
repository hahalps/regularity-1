{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Regularity.Automata.NFAe
  ( NFAe()
  , shiftBy
  )
where

import Prelude hiding (seq)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

import Data.List (intercalate)

import qualified Data.Text as T

import Regularity.Automata hiding (StateId, shiftStateIdBy)

-- epsilon transitions: Nothing is epsilon; Just c is a character

type StateId = Int

type StateMap a = IntMap a

shiftStateIdBy :: StateId -> Int -> StateId
shiftStateIdBy = (+)

type StateSet = IntSet

data NFAe =
  NFAe { states :: !StateSet
       , delta :: !(StateMap (Map (Maybe Char) StateSet))
       , accepting :: !StateSet
       , startState :: !StateId
       }

instance Automaton NFAe where
  accepts = Regularity.Automata.NFAe.accepts

  aempty = empty
  aepsilon = epsilon
  achar = char
  aseq = seq
  aalt = alt
  astar = star

transitionsFor :: NFAe -> StateId -> Map (Maybe Char) StateSet
transitionsFor a si = IntMap.findWithDefault Map.empty si (delta a)

instance Show NFAe where
  show a =
    unlines $ [ "STATES: " ++ intercalate ", " (map show $ Set.toAscList $ states a)
              , "START STATE: " ++ (show $ startState a)
              , "ACCEPTING: " ++ intercalate ", " (map show $ Set.toAscList $ accepting a)
              , "TRANSITIONS:"
              ] ++
    IntMap.foldrWithKey
    (\si trans ls ->
       let prefix = show si ++ ": "
           spaces = replicate (length prefix) ' '
       in
         prefix :
         Map.foldrWithKey
         (\mc sis ls' ->
             let input = case mc of
                           Nothing -> "ε"
                           Just c -> ['\'', c, '\'']
             in
               (spaces ++ input ++ " |-> " ++ intercalate ", " (map show $ Set.toAscList $ sis)) :
               ls'
             )
         ls
         trans)
    []
    (delta a)

shiftBy :: NFAe -> Int -> NFAe
shiftBy a n =
  NFAe { states = Set.map (`shiftStateIdBy` n) (states a)
       , delta = IntMap.foldrWithKey
                 (\si trans delta' ->
                     IntMap.insert (si `shiftStateIdBy` n) (Map.map (Set.map (`shiftStateIdBy` n)) trans) delta')
                 IntMap.empty
                 (delta a)
       , startState = startState a `shiftStateIdBy` n
       , accepting = Set.map (`shiftStateIdBy` n) (accepting a)
       }

maxStateId :: NFAe -> Int
maxStateId a = Set.findMax $ states a

accepts :: NFAe -> T.Text -> Bool
accepts !a !s = Set.foldr (\si found -> found || (si `Set.member` accepting a)) False $ run a s

run :: NFAe -> T.Text -> StateSet
run !a = runIn $ epsilonSteps a $ Set.singleton $ startState a
  where
    runIn :: StateSet -> T.Text -> StateSet
    runIn currentStates s =
      case T.uncons s of
        Nothing -> currentStates
        Just (c, s') -> runIn (step a currentStates c) s'

step :: NFAe -> StateSet -> Char -> StateSet
step a currentStates c =
  let nextStatesFor si = Map.findWithDefault Set.empty (Just c) (a `transitionsFor` si)
      nextStates =
        Set.foldr (\si next -> Set.union (nextStatesFor si) next)
        Set.empty currentStates
  in
    epsilonSteps a nextStates

epsilonSteps :: NFAe -> StateSet -> StateSet
epsilonSteps a initial = dfs (Set.toList initial) initial
  where
    dfs :: [StateId] -> StateSet -> StateSet
    dfs []       seen = seen
    dfs (si:sis) seen =
      let next  = Map.findWithDefault Set.empty Nothing (a `transitionsFor` si)
          new   = Set.filter (\si' -> not (si' `Set.member` seen)) next
      in
        dfs (Set.toList new ++ sis) (new `Set.union` seen)
                
empty :: NFAe
empty =
  let id0 = 0
  in
  NFAe { states = Set.singleton id0
       , delta = IntMap.singleton id0 Map.empty
       , startState = id0
       , accepting = Set.empty
       }

epsilon :: NFAe
epsilon =
  let id0 = 0
  in
    NFAe { states = Set.singleton id0
         , delta = IntMap.singleton id0 Map.empty
         , startState = id0
         , accepting = Set.singleton id0
         }

char :: Char -> NFAe
char c =
  let id0 = 0
      id1 = 1
  in
    NFAe { states = Set.fromList [id0, id1]
         , delta = IntMap.singleton id0 (Map.singleton (Just c) (Set.singleton id1))
         , startState = id0
         , accepting = Set.singleton id1
         }

seq :: NFAe -> NFAe -> NFAe
seq !a1 !a2 =
    let a2' = a2 `shiftBy` (maxStateId a1 + 1)
        delta1' = Set.foldr
                    (\si delta' ->
                       IntMap.insertWith (Map.unionWith Set.union)
                         si (Map.singleton Nothing (Set.singleton $ startState a2')) delta')
                    (delta a1)
                    (accepting a1)
    in
      NFAe { states = states a1 `Set.union` states a2'
           , delta = delta1' `IntMap.union` delta a2'
           , startState = startState a1
           , accepting = accepting a2'
           }

alt :: NFAe -> NFAe -> NFAe
alt !a1 !a2 =
  let a1' = a1 `shiftBy` 1
      a2' = a2 `shiftBy` (maxStateId a1' + 1)
      id0 = 0
  in
    NFAe { states = Set.insert id0 (states a1' `Set.union` states a2')
              , delta = IntMap.insert id0
                          (Map.singleton Nothing (Set.fromList [ startState a1'
                                                               , startState a2'
                                                               ]))
                        (delta a1' `IntMap.union` delta a2')
              , startState = id0
              , accepting = accepting a1' `Set.union` accepting a2'
              }

star :: NFAe -> NFAe
star !a =
  let delta' = Set.foldr
                 (\si delta'' ->
                    IntMap.insertWith (Map.unionWith Set.union)
                      si (Map.singleton Nothing (Set.singleton $ startState a)) delta'')
                 (delta a)
                 (accepting a)
  in
    NFAe { states = states a
              , delta = delta'
              , startState = startState a
              , accepting = Set.insert (startState a) (accepting a)
              }
