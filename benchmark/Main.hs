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
    [ starTests R.matches $ Star (Char 'a')
    , starTests R.matches $ Star (Alt Epsilon (Char 'a'))
    ]
  , bgroup "automata"
    [ starTests (A.accepts . A.fromRegex) $ Star (Char 'a')
    , starTests (A.accepts . A.fromRegex) $ Star (Alt Epsilon (Char 'a'))
    ]
  ]

starTests :: Show a => (a -> T.Text -> b) -> a -> Benchmark
starTests matcher re =
  bgroup (show re)
  [ bench "on aaaa" $
    whnf (matcher re) "aaaa"
  , bench "on a^10" $
    whnf (matcher re) $ T.replicate 10 $ T.singleton 'a'
  , bench "on a^20" $
    whnf (matcher re) $ T.replicate 20 $ T.singleton 'a'
  ]
  
