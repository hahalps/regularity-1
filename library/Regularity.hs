{-# LANGUAGE OverloadedStrings #-}

-- | The top-level Regularity module.
module Regularity
  ( main
  , module Regularity.Regex
  , module Regularity.Automata
  )
where

import Regularity.Regex
import Regularity.Automata

-- | Default entry point.
main :: IO ()
main =
  putStrLn "regularity: coming soon"
