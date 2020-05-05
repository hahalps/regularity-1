{-# LANGUAGE OverloadedStrings #-}

import Criterion.Main

import Data.Text (Text)
import qualified Data.Text as T

import Regularity.Automata
import Regularity.Automata.NFAe (NFAe)
import Regularity.Automata.NFA (NFA)
import Regularity.Regex as R

acceptsNFAe :: Regex -> Text -> Bool
acceptsNFAe re t = accepts (fromRegex re :: NFAe) t

acceptsNFA :: Regex -> Text -> Bool
acceptsNFA re t = accepts (fromRegex re :: NFA) t

main :: IO ()
main = defaultMain
  [ starTests "regex" R.matches
  , starTests "NFAe" acceptsNFAe
  , starTests "NFA" acceptsNFA
  , bgroup "scaling" $
    let re = Star (Alt (Char 'a') (Char 'a'))
        ss = [ T.replicate 50 $ T.singleton 'a'
             , T.replicate 100 $ T.singleton 'a'
             ]
    in
    [ matcherTests re acceptsNFA ss
    ]
  ]

starTests :: String -> (Regex -> Text -> b) -> Benchmark
starTests name matcher =
  bgroup (name ++ " star") $
  [ matcherTests (Star (Char 'a')) matcher ["aaaa", "aaaaaaaaaa", "aaaaaaaaaaaaaaaaaa"]
  , matcherTests (Star (Alt Epsilon (Char 'a'))) matcher ["aaaa", "aaaaaaaaaa", "aaaaaaaaaaaaaaaaaa"]
  ]

matcherTests :: Show a => a -> (a -> Text -> b) -> [Text] -> Benchmark
matcherTests re matcher strings =
  bgroup (show re) $
  map (\s -> bench ("on " ++ T.unpack s) $ whnf (matcher re) s) strings

