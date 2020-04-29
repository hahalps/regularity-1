{-# LANGUAGE OverloadedStrings, BangPatterns #-}
module Regularity.Automata.NFAe
  ( NFAe()
  , shiftBy
  )
where

import Prelude hiding (seq)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (intercalate)

import qualified Data.Text as T

import Regularity.Regex
import Regularity.Automata

{- PLAN

   - TODO: epsilon elimination
     - determinize

   - use real intmaps rather than Map

   - TODO report criterion bug (bad HTML output when there's a newline in the test name)
-}

-- epsilon transitions: Nothing is epsilon; Just c is a character

data NFAe =
  NFAe { states :: !(Set StateId)
            , delta :: !(Map StateId (Map (Maybe Char) (Set StateId)))
            , accepting :: !(Set StateId)
            , startState :: !StateId
            }

instance Automaton NFAe where
  accepts = Regularity.Automata.NFAe.accepts

  empty = Regularity.Automata.NFAe.empty
  epsilon = Regularity.Automata.NFAe.epsilon
  char = Regularity.Automata.NFAe.char
  seq = Regularity.Automata.NFAe.seq
  alt = Regularity.Automata.NFAe.alt
  star = Regularity.Automata.NFAe.star

transitionsFor :: NFAe -> StateId -> Map (Maybe Char) (Set StateId)
transitionsFor a si = Map.findWithDefault Map.empty si (delta a)

instance Show NFAe where
  show a =
    unlines $ [ "STATES: " ++ intercalate ", " (map show $ Set.toAscList $ states a)
              , "START STATE: " ++ (show $ startState a)
              , "ACCEPTING: " ++ intercalate ", " (map show $ Set.toAscList $ accepting a)
              , "TRANSITIONS:"
              ] ++
    Map.foldrWithKey
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
            , delta = Map.foldrWithKey
                      (\si trans delta' ->
                         Map.insert (si `shiftStateIdBy` n) (Map.map (Set.map (`shiftStateIdBy` n)) trans) delta')
                      Map.empty
                      (delta a)
            , startState = startState a `shiftStateIdBy` n
            , accepting = Set.map (`shiftStateIdBy` n) (accepting a)
            }

maxStateId :: NFAe -> Int
maxStateId a = getStateId $ maximum $ states a

accepts :: NFAe -> T.Text -> Bool
accepts !a !s = any (\si -> si `Set.member` accepting a) $ run a s

run :: NFAe -> T.Text -> Set StateId
run !a = runIn $ epsilonSteps a $ Set.singleton $ startState a
  where
    runIn :: Set StateId -> T.Text -> Set StateId
    runIn currentStates s =
      case T.uncons s of
        Nothing -> currentStates
        Just (c, s') -> runIn (step a currentStates c) s'

step :: NFAe -> Set StateId -> Char -> Set StateId
step a currentStates c =
  let nextStatesFor si = Map.findWithDefault Set.empty (Just c) (a `transitionsFor` si)
      nextStates =
        Set.foldr (\si next -> Set.union (nextStatesFor si) next)
        Set.empty currentStates
  in
    epsilonSteps a nextStates

epsilonSteps :: NFAe -> Set StateId -> Set StateId
epsilonSteps a initial = dfs (Set.toList initial) initial
  where
    dfs :: [StateId] -> Set StateId -> Set StateId
    dfs []       seen = seen
    dfs (si:sis) seen =
      let next  = Map.findWithDefault Set.empty Nothing (a `transitionsFor` si)
          new   = Set.filter (\si' -> not (si' `Set.member` seen)) next
      in
        dfs (Set.toList new ++ sis) (new `Set.union` seen)
                
empty :: NFAe
empty =
  let id0 = StateId 0
  in
  NFAe { states = Set.singleton id0
            , delta = Map.singleton id0 Map.empty
            , startState = id0
            , accepting = Set.empty
            }

epsilon :: NFAe
epsilon =
  let id0 = StateId 0
  in
    NFAe { states = Set.singleton id0
              , delta = Map.singleton id0 Map.empty
              , startState = id0
              , accepting = Set.singleton id0
              }

char :: Char -> NFAe
char c =
  let id0 = StateId 0
      id1 = StateId 1
  in
    NFAe { states = Set.fromList [id0, id1]
              , delta = Map.singleton id0 (Map.singleton (Just c) (Set.singleton id1))
              , startState = id0
              , accepting = Set.singleton id1
              }

seq :: NFAe -> NFAe -> NFAe
seq !a1 !a2 =
    let a2' = a2 `shiftBy` (maxStateId a1 + 1)
        delta1' = Set.foldr
                    (\si delta' ->
                       Map.insertWith (Map.unionWith Set.union)
                         si (Map.singleton Nothing (Set.singleton $ startState a2')) delta')
                    (delta a1)
                    (accepting a1)
    in
      NFAe { states = states a1 `Set.union` states a2'
                , delta = delta1' `Map.union` delta a2'
                , startState = startState a1
                , accepting = accepting a2'
                }

alt :: NFAe -> NFAe -> NFAe
alt !a1 !a2 =
  let a1' = a1 `shiftBy` 1
      a2' = a2 `shiftBy` (maxStateId a1' + 1)
      id0 = StateId 0
  in
    NFAe { states = Set.insert id0 (states a1' `Set.union` states a2')
              , delta = Map.insert id0
                          (Map.singleton Nothing (Set.fromList [ startState a1'
                                                               , startState a2'
                                                               ]))
                        (delta a1' `Map.union` delta a2')
              , startState = id0
              , accepting = accepting a1' `Set.union` accepting a2'
              }

star :: NFAe -> NFAe
star !a =
  let delta' = Set.foldr
                 (\si delta'' ->
                    Map.insertWith (Map.unionWith Set.union)
                      si (Map.singleton Nothing (Set.singleton $ startState a)) delta'')
                 (delta a)
                 (accepting a)
  in
    NFAe { states = states a
              , delta = delta'
              , startState = startState a
              , accepting = Set.insert (startState a) (accepting a)
              }
