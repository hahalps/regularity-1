{-# LANGUAGE OverloadedStrings #-}

-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import Regularity.Automata as A
import Regularity.Regex as R

import qualified Data.Text as T

main :: IO ()
main = defaultMain
  [ bgroup "regex"
    [ bench "regex a* on aaaa" $
      whnf (matches $ Star (Char 'a')) "aaaa"
    ]
  , bgroup "automata"
    [ bench "a* on aaaa" $
      let a = star (char 'a') in
      whnf (accepts a) "aaaa"
    ]
  ]
