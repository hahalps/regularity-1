{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Regularity.Automata.NFAe
  ( NFAe(..)
  , shiftBy
  , transitionsFor
  , epsilonSteps
  )
where

import Prelude hiding (seq)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

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
    unlines $ [ "STATES: " ++ intercalate ", " (map show $ IntSet.toAscList $ states a)
              , "START STATE: " ++ (show $ startState a)
              , "ACCEPTING: " ++ intercalate ", " (map show $ IntSet.toAscList $ accepting a)
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
               (spaces ++ input ++ " |-> " ++ intercalate ", " (map show $ IntSet.toAscList $ sis)) :
               ls'
             )
         ls
         trans)
    []
    (delta a)

shiftBy :: NFAe -> Int -> NFAe
shiftBy a n =
  NFAe { states = IntSet.map (`shiftStateIdBy` n) (states a)
       , delta = IntMap.foldrWithKey
                 (\si trans delta' ->
                     IntMap.insert (si `shiftStateIdBy` n) (Map.map (IntSet.map (`shiftStateIdBy` n)) trans) delta')
                 IntMap.empty
                 (delta a)
       , startState = startState a `shiftStateIdBy` n
       , accepting = IntSet.map (`shiftStateIdBy` n) (accepting a)
       }

maxStateId :: NFAe -> Int
maxStateId a = IntSet.findMax $ states a

accepts :: NFAe -> T.Text -> Bool
accepts !a !s = IntSet.foldr (\si found -> found || (si `IntSet.member` accepting a)) False $ run a s

run :: NFAe -> T.Text -> StateSet
run !a s = T.foldl (step a) (epsilonSteps a $ IntSet.singleton $ startState a) s

step :: NFAe -> StateSet -> Char -> StateSet
step a currentStates c =
  let nextStatesFor si = Map.findWithDefault IntSet.empty (Just c) (a `transitionsFor` si)
      nextStates =
        IntSet.foldr (\si next -> IntSet.union (nextStatesFor si) next)
        IntSet.empty currentStates
  in
    epsilonSteps a nextStates

epsilonSteps :: NFAe -> StateSet -> StateSet
epsilonSteps a initial = dfs (IntSet.toList initial) initial
  where
    dfs :: [StateId] -> StateSet -> StateSet
    dfs []       seen = seen
    dfs (si:sis) seen =
      let next  = Map.findWithDefault IntSet.empty Nothing (a `transitionsFor` si)
          new   = IntSet.filter (\si' -> not (si' `IntSet.member` seen)) next
      in
        dfs (IntSet.toList new ++ sis) (new `IntSet.union` seen)
                
empty :: NFAe
empty =
  let id0 = 0
  in
  NFAe { states = IntSet.singleton id0
       , delta = IntMap.singleton id0 Map.empty
       , startState = id0
       , accepting = IntSet.empty
       }

epsilon :: NFAe
epsilon =
  let id0 = 0
  in
    NFAe { states = IntSet.singleton id0
         , delta = IntMap.singleton id0 Map.empty
         , startState = id0
         , accepting = IntSet.singleton id0
         }

char :: Char -> NFAe
char c =
  let id0 = 0
      id1 = 1
  in
    NFAe { states = IntSet.fromList [id0, id1]
         , delta = IntMap.singleton id0 (Map.singleton (Just c) (IntSet.singleton id1))
         , startState = id0
         , accepting = IntSet.singleton id1
         }

seq :: NFAe -> NFAe -> NFAe
seq !a1 !a2 =
    let a2' = a2 `shiftBy` (maxStateId a1 + 1)
        delta1' = IntSet.foldr
                    (\si delta' ->
                       IntMap.insertWith (Map.unionWith IntSet.union)
                         si (Map.singleton Nothing (IntSet.singleton $ startState a2')) delta')
                    (delta a1)
                    (accepting a1)
    in
      NFAe { states = states a1 `IntSet.union` states a2'
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
    NFAe { states = IntSet.insert id0 (states a1' `IntSet.union` states a2')
              , delta = IntMap.insert id0
                          (Map.singleton Nothing (IntSet.fromList [ startState a1'
                                                               , startState a2'
                                                               ]))
                        (delta a1' `IntMap.union` delta a2')
              , startState = id0
              , accepting = accepting a1' `IntSet.union` accepting a2'
              }

star :: NFAe -> NFAe
star !a =
  let delta' = IntSet.foldr
                 (\si delta'' ->
                    IntMap.insertWith (Map.unionWith IntSet.union)
                      si (Map.singleton Nothing (IntSet.singleton $ startState a)) delta'')
                 (delta a)
                 (accepting a)
  in
    NFAe { states = states a
              , delta = delta'
              , startState = startState a
              , accepting = IntSet.insert (startState a) (accepting a)
              }
