{-# LANGUAGE OverloadedStrings #-}

-- | The top-level Regularity module.
module Regularity
  ( main
  , module Regularity.Regex
  )
where

import Regularity.Regex

{- next time:

   - define automata
   - convert regex to automata
   - matching via automata
   - SPEED TEST LET'S GO!!!11!!!!11111
-}

-- | Default entry point.
main :: IO ()
main =
  putStrLn "regularity: coming soon"
