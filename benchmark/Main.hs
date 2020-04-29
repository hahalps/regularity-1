{-# LANGUAGE OverloadedStrings #-}

-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main

import Regularity.Automata
import Regularity.Automata.NFAe (NFAe)
import qualified Regularity.Automata.NFAeI as NFAeI
import Regularity.Automata.NFA (NFA)
import qualified Regularity.Automata.NFAI as NFAI
import Regularity.Regex as R

import Data.Text (Text)
import qualified Data.Text as T

acceptsNFAe :: Regex -> Text -> Bool
acceptsNFAe re t = accepts (fromRegex re :: NFAe) t

acceptsNFAeI :: Regex -> Text -> Bool
acceptsNFAeI re t = accepts (fromRegex re :: NFAeI.NFAe) t

acceptsNFA :: Regex -> Text -> Bool
acceptsNFA re t = accepts (fromRegex re :: NFA) t

acceptsNFAI :: Regex -> Text -> Bool
acceptsNFAI re t = accepts (fromRegex re :: NFAI.NFA) t

main :: IO ()
main = defaultMain
  [ bgroup "regex"
    [ starTests R.matches $ Star (Char 'a')
    , starTests R.matches $ Star (Alt Epsilon (Char 'a'))
    ]
  , bgroup "NFAe"
    [ starTests acceptsNFAe $ Star (Char 'a')
    , starTests acceptsNFAe $ Star (Alt Epsilon (Char 'a'))
    ]
  , bgroup "NFAeI"
    [ starTests acceptsNFAeI $ Star (Char 'a')
    , starTests acceptsNFAeI $ Star (Alt Epsilon (Char 'a'))
    ]
  , bgroup "NFA"
    [ starTests acceptsNFA $ Star (Char 'a')
    , starTests acceptsNFA $ Star (Alt Epsilon (Char 'a'))
    ]
  , bgroup "NFAI"
    [ starTests acceptsNFA $ Star (Char 'a')
    , starTests acceptsNFA $ Star (Alt Epsilon (Char 'a'))
    ]
  ]

starTests :: Show a => (a -> Text -> b) -> a -> Benchmark
starTests matcher re =
  bgroup (show re)
  [ bench "on aaaa" $
    whnf (matcher re) "aaaa"
  , bench "on a^10" $
    whnf (matcher re) $ T.replicate 10 $ T.singleton 'a'
  , bench "on a^20" $
    whnf (matcher re) $ T.replicate 20 $ T.singleton 'a'
  ]
  
