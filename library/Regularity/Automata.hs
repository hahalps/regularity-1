{-# LANGUAGE OverloadedStrings #-}
module Regularity.Automata
where

import Regularity.Regex

import Test.QuickCheck

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List (intercalate)

import qualified Data.Text as T

{- PLAN

   - *** SPEED TEST LET'S GO!!!11!!!!11111
     + explicitly work with state sets in run
     + make sure laziness isn't hurting us (strictness!)

   - epsilon elimination
     - determinize

   - use real intmaps rather than Map

   - validity testing of automata

   - better testing
-}

newtype StateId = StateId { getStateId :: Int }
  deriving (Eq, Ord)

instance Show StateId where
  show (StateId i) = "s_" ++ show i

-- epsilon transitions: Nothing is epsilon; Just c is a character

data Automaton =
  Automaton { states :: Set StateId
            , delta :: Map StateId (Map (Maybe Char) (Set StateId))
            , accepting :: Set StateId
            , startState :: StateId
            }

transitionsFor :: Automaton -> StateId -> Map (Maybe Char) (Set StateId)
transitionsFor a si = Map.findWithDefault Map.empty si (delta a)

instance Show Automaton where
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
                           Just c -> [c]
             in
               (spaces ++ input ++ " |-> " ++ intercalate ", " (map show $ Set.toAscList $ sis)) :
               ls'
             )
         ls
         trans)
    []
    (delta a)

shiftBy :: StateId -> Int -> StateId
shiftBy (StateId i) n = StateId $ i + n

shiftAutomatonBy :: Automaton -> Int -> Automaton
shiftAutomatonBy a n =
  Automaton { states = Set.map (`shiftBy` n) (states a)
            , delta = Map.foldrWithKey
                      (\si trans delta' ->
                         Map.insert (si `shiftBy` n) (Map.map (Set.map (`shiftBy` n)) trans) delta')
                      Map.empty
                      (delta a)
            , startState = startState a `shiftBy` n
            , accepting = Set.map (`shiftBy` n) (accepting a)
            }

maxStateId :: Automaton -> Int
maxStateId a = getStateId $ maximum $ states a

accepts :: Automaton -> T.Text -> Bool
accepts a s = any (\si -> si `Set.member` accepting a) $ run a s

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
  let nextStatesFor si = Map.findWithDefault Set.empty (Just c) (a `transitionsFor` si)
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
      let next  = Map.findWithDefault Set.empty Nothing (a `transitionsFor` si)
          new   = Set.filter (\si' -> not (si' `Set.member` seen)) next
      in
        dfs (Set.toList new ++ sis) (new `Set.union` seen)
                
empty :: Automaton
empty =
  let id0 = StateId 0
  in
  Automaton { states = Set.singleton id0
            , delta = Map.singleton id0 Map.empty
            , startState = id0
            , accepting = Set.empty
            }

epsilon :: Automaton
epsilon =
  let id0 = StateId 0
  in
    Automaton { states = Set.singleton id0
              , delta = Map.singleton id0 Map.empty
              , startState = id0
              , accepting = Set.singleton id0
              }

char :: Char -> Automaton
char c =
  let id0 = StateId 0
      id1 = StateId 1
  in
    Automaton { states = Set.fromList [id0, id1]
              , delta = Map.singleton id0 (Map.singleton (Just c) (Set.singleton id1))
              , startState = id0
              , accepting = Set.singleton id1
              }

seq :: Automaton -> Automaton -> Automaton
seq a1 a2 =
    let a2' = a2 `shiftAutomatonBy` (maxStateId a1 + 1)
        delta1' = Set.foldr
                    (\si delta' ->
                       Map.insertWith (Map.unionWith Set.union)
                         si (Map.singleton Nothing (Set.singleton $ startState a2')) delta')
                    (delta a1)
                    (accepting a1)
    in
      Automaton { states = states a1 `Set.union` states a2'
                , delta = delta1' `Map.union` delta a2'
                , startState = startState a1
                , accepting = accepting a2'
                }

alt :: Automaton -> Automaton -> Automaton
alt a1 a2 =
  let a1' = a1 `shiftAutomatonBy` 1
      a2' = a2 `shiftAutomatonBy` (maxStateId a1' + 1)
      id0 = StateId 0
  in
    Automaton { states = Set.insert id0 (states a1' `Set.union` states a2')
              , delta = Map.insert id0
                          (Map.singleton Nothing (Set.fromList [ startState a1'
                                                               , startState a2'
                                                               ]))
                        (delta a1' `Map.union` delta a2')
              , startState = id0
              , accepting = accepting a1' `Set.union` accepting a2'
              }                                                        

star :: Automaton -> Automaton
star a =
  let
    delta' =  Set.foldr
                    (\si delta'' ->
                       Map.insertWith (Map.unionWith Set.union)
                         si (Map.singleton Nothing (Set.singleton $ startState a)) delta'')
                    (delta a)
                    (accepting a)
  in
    Automaton { states = states a
              , delta = delta'
              , startState = startState a
              , accepting = Set.insert (startState a) (accepting a)
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
