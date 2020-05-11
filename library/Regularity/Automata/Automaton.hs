{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Regularity.Automata.Automaton
  ( Automaton(..)
  , run
  , accepts
  )
where

import Data.Text (Text)
import qualified Data.Text as T

class Automaton a s | a -> s where
  initialState :: a -> s
  step :: a -> s -> Char -> s
  accepting :: a -> s -> Bool

accepts :: Automaton a s => a -> Text -> Bool
accepts a str = accepting a $ run a str

run :: Automaton a s => a -> Text -> s
run a str = T.foldl (step a) (initialState a) str
