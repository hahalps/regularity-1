{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Regularity.Automata.NFA
  ( NFA()
  , shiftBy
  )
where

import Prelude hiding (seq, init)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

import Data.List (intercalate)

import qualified Data.Text as T

import Regularity.Automata hiding (StateId, shiftStateIdBy)

type StateId = Int

type StateMap a = IntMap a

shiftStateIdBy :: StateId -> Int -> StateId
shiftStateIdBy = (+)

type StateSet = IntSet

data NFA =
  NFA { states :: !StateSet
      , delta :: !(StateMap (Map Char StateSet))
      , accepting :: !StateSet
      , startState :: !StateId
      }

instance Automaton NFA where
  accepts = Regularity.Automata.NFA.accepts

  aempty = empty
  aepsilon = epsilon
  achar = char
  aseq = seq
  aalt = alt
  astar = star

transitionsFor :: NFA -> StateId -> Map Char StateSet
transitionsFor a si = IntMap.findWithDefault Map.empty si (delta a)

instance Show NFA where
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
         (\c sis ls' ->
             let input = ['\'', c, '\'']
             in
               (spaces ++ input ++ " |-> " ++ intercalate ", " (map show $ Set.toAscList $ sis)) :
               ls'
             )
         ls
         trans)
    []
    (delta a)

shiftBy :: NFA -> Int -> NFA
shiftBy a n =
  NFA { states = Set.map (`shiftStateIdBy` n) (states a)
      , delta = IntMap.foldrWithKey
                (\si trans delta' ->
                    IntMap.insert (si `shiftStateIdBy` n)
                      (Map.map (Set.map (`shiftStateIdBy` n)) trans) delta')
                IntMap.empty
                (delta a)
      , startState = startState a `shiftStateIdBy` n
      , accepting = Set.map (`shiftStateIdBy` n) (accepting a)
      }

maxStateId :: NFA -> Int
maxStateId a = Set.findMax $ states a

accepts :: NFA -> T.Text -> Bool
accepts !a !s = Set.foldr (\si found -> found || (si `Set.member` accepting a)) False $ run a s

run :: NFA -> T.Text -> StateSet
run !a = runIn $ Set.singleton $ startState a
  where
    runIn :: StateSet -> T.Text -> StateSet
    runIn currentStates s =
      case T.uncons s of
        Nothing -> currentStates
        Just (c, s') -> runIn (step a currentStates c) s'

step :: NFA -> StateSet -> Char -> StateSet
step a currentStates c =
  let nextStatesFor si =
        Map.findWithDefault Set.empty c (a `transitionsFor` si)
  in
    Set.foldr (\si next -> Set.union (nextStatesFor si) next)
        Set.empty currentStates

empty :: NFA
empty =
  let id0 = 0
  in
  NFA { states = Set.singleton id0
      , delta = IntMap.singleton id0 Map.empty
      , startState = id0
      , accepting = Set.empty
      }

epsilon :: NFA
epsilon =
  let id0 = 0
  in
    NFA { states = Set.singleton id0
        , delta = IntMap.singleton id0 Map.empty
        , startState = id0
        , accepting = Set.singleton id0
        }

char :: Char -> NFA
char c =
  let id0 = 0
      id1 = 1
  in
    NFA { states = Set.fromList [id0, id1]
        , delta = IntMap.singleton id0 (Map.singleton c (Set.singleton id1))
        , startState = id0
        , accepting = Set.singleton id1
        }

seq :: NFA -> NFA -> NFA
seq !a1 !a2 =
  -- CORE IDEA: accepting states in a1 simulate a2's start state
  let a2' = a2 `shiftBy` (maxStateId a1 + 1)
      a2Inits = a2' `transitionsFor` startState a2'      
      delta1' = Set.foldr
                  (\si delta' ->
                     IntMap.insertWith (Map.unionWith Set.union)
                       si a2Inits delta')
                  (delta a1)
                  (accepting a1)
  in
    NFA { states = states a1 `Set.union` states a2'
        , delta = delta1' `IntMap.union` delta a2'
        , startState = startState a1
        , accepting =
            {- make sure that a1's accepting states
               are accepting if a2's startState is accepting
             -}
            (if startState a2' `Set.member` accepting a2'
                 then accepting a1
                 else Set.empty)
                `Set.union`
                accepting a2'
              }

alt :: NFA -> NFA -> NFA
alt !a1 !a2 =
  let a1' = a1 `shiftBy` 1
      a2' = a2 `shiftBy` (maxStateId a1' + 1)
      id0 = 0
      a1Init = a1' `transitionsFor` startState a1'
      a2Init = a2' `transitionsFor` startState a2'
      init   = Map.unionWith Set.union a1Init a2Init
  in
    NFA { states = Set.insert id0 (states a1' `Set.union` states a2')
        , delta = IntMap.insert id0 init (delta a1' `IntMap.union` delta a2')
        , startState = id0
        , accepting =
            Set.unions
            [ if startState a1' `Set.member` accepting a1' || startState a2' `Set.member` accepting a2'
              then Set.singleton id0
              else Set.empty
            , accepting a1'
            , accepting a2'
            ]
        }

star :: NFA -> NFA
star !a =
  let
    init   = a `transitionsFor` startState a
    delta' = Set.foldr
               (\si delta'' ->
                  IntMap.insertWith (Map.unionWith Set.union)
                    si init delta'')
               (delta a)
               (accepting a)
  in
    NFA { states = states a
              , delta = delta'
              , startState = startState a
              , accepting = Set.insert (startState a) (accepting a)
              }

-- TODO fromNFAe
