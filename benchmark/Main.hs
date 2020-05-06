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
  , starTests "regex (Brzozowski derivative)" R.dMatches
  , starTests "NFA" acceptsNFA
  , starTests "NFAe" acceptsNFAe
  , scalingTests "regex" R.matches
  , scalingTests "regex (Brzozowski derivative)" R.dMatches
  , scalingTests "NFA" acceptsNFA
  , scalingTests "NFAe" acceptsNFAe
  ]

scalingTests :: String -> (Regex -> Text -> b) -> Benchmark
scalingTests name matcher =
  let re = Star (Alt (Char 'a') (Char 'a'))
      ss = [ T.replicate 50 $ T.singleton 'a'
           , T.replicate 100 $ T.singleton 'a'
           ]
  in
    bgroup (name ++ " scaling") 
    [ matcherTests re matcher ss
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
  map (\s -> bench ("on " ++ describe s) $ whnf (matcher re) s) strings

describe :: Text -> String
describe t =
  if T.length t < 10
  then T.unpack t
  else T.unpack (T.take 4 t) ++ "... (length " ++ show (T.length t) ++ ")"
