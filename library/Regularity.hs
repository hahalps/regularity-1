{-# LANGUAGE OverloadedStrings #-}

-- | The top-level Regularity module.
module Regularity
  ( main
  , module Regularity.Regex
  )
where

import Regularity.Regex

-- | Default entry point.
main :: IO ()
main =
  putStrLn "regularity: coming soon"
