{-# LANGUAGE MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module Regularity.Brzozowski
  ( Brzozowski(..)
  , deriv
  , derivS
  )
where

import Prelude hiding (seq)

import qualified Regularity.Regular as Regular

import Regularity.Regex

import Regularity.Automata.Automaton


-- | Brzozowski derivatives

{- idea: Brzozowski automata

  A(E)

  Q \subseteq Regex   IMPLICIT STATES
  s0 = E
  delta(s, c) = deriv_c(s)
  accepting(s) = nullable s

-}

newtype Brzozowski = Brzozowski { brzozowskiRegex :: Regex }
  deriving (Eq, Ord, Show, Regular.Regular)

instance Automaton Brzozowski Regex where
  initialState a = brzozowskiRegex a

  step _a re c = deriv c re

  accepting _a re = nullable re

-- re `matches` c:s iff deriv c re `matches` s
deriv :: Char -> Regex -> Regex
deriv _c Empty         = empty
deriv _c Epsilon       = empty
deriv  c (Char c')     = if c == c' then epsilon else empty
deriv  c (Seq re1 re2) = alt (seq (deriv c re1) re2) (if nullable re1 then deriv c re2 else empty)
deriv  c (Alt re1 re2) = alt (deriv c re1) (deriv c re2)
deriv  c (Star re)     = seq (deriv c re) (star re)

-- derivative that doesn't use smart constructors (big blowup)
derivS :: Char -> Regex -> Regex
derivS _c Empty         = Empty
derivS _c Epsilon       = Empty
derivS  c (Char c')     = if c == c' then Epsilon else Empty
derivS  c (Seq re1 re2) = Alt (Seq (derivS c re1) re2) (if nullable re1 then derivS c re2 else Empty)
derivS  c (Alt re1 re2) = Alt (derivS c re1) (derivS c re2)
derivS  c (Star re)     = Seq (derivS c re) (Star re)
