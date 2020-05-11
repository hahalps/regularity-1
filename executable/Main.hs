import Regularity.Regex (Alphabet, Regex)
import qualified Regularity.Regex as R
import qualified Regularity.Brzozowski as B

import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  putStrLn "name,alphabet,initial size,nth derivative,largest derivative regex"
  
  let sigma = Set.fromList "ab"
{-  nthDerivativeSizes sigma 2 3
  nthDerivativeSizes sigma 3 3
  nthDerivativeSizes sigma 4 3
  nthDerivativeSizes sigma 5 3 -}
  nthDerivativeSizes sigma 6 10

{-
  let sigma = Set.fromList "abc"
  nthDerivativeSizes sigma 2 3
  nthDerivativeSizes sigma 3 3
  nthDerivativeSizes sigma 4 3
  nthDerivativeSizes sigma 5 3
  nthDerivativeSizes sigma 6 3
-}

nthDerivativeSizes :: Alphabet -> Int -> Int -> IO ()
nthDerivativeSizes sigma size totalDerivs = loop totalDerivs (R.regexesOfSize sigma size) (R.regexesOfSize sigma size)
  where loop 0 _res _resS = return ()
        loop n  res  resS = do
          -- compute smart and stupid derivatives
          let derivs  = Set.unions $ Set.map (allDerivs B.deriv  sigma) res
          let derivsS = Set.unions $ Set.map (allDerivs B.derivS sigma) resS

          let sizes  = Set.map R.size derivs
          let sizesS = Set.map R.size derivsS
          putStrLn ("smart," ++ Set.toList sigma ++ "," ++ show size ++ "," ++ show (totalDerivs - n + 1) ++ "," ++ show (maximum sizes))
          putStrLn ("plain," ++ Set.toList sigma ++ "," ++ show size ++ "," ++ show (totalDerivs - n + 1) ++ "," ++ show (maximum sizesS))
          loop (n-1) derivs derivsS

singleStepDerivativeSizes :: Alphabet -> Int -> IO ()
singleStepDerivativeSizes sigma size = do
  let res = R.regexesOfSize sigma size
  let sizes  = derivSizes B.deriv  sigma res
  let sizesS = derivSizes B.derivS sigma res
  putStrLn ("smart," ++ Set.toList sigma ++ "," ++ show size ++ "," ++ show (maximum sizes))
  putStrLn ("plain," ++ Set.toList sigma ++ "," ++ show size ++ "," ++ show (maximum sizesS))

{- generate a bunch of regexes of a given size
     in a given alphabet
   compute their derivative wrt each char in the alphabet
   see what sizes the derivatives are

   Q: should we iterate?
      yes... a little.
-}

type Derivative = Char -> Regex -> Regex

allDerivs :: Derivative -> Alphabet -> Regex -> Set Regex
allDerivs d sigma re =
  Set.foldr
    (\c derivs ->
        Set.insert (d c re) derivs)
    Set.empty
    sigma

allNthDerivs :: Int -> Derivative -> Alphabet -> Regex -> Set Regex
allNthDerivs 0 _deriv _sigma _re = Set.empty
allNthDerivs n  deriv  sigma  re =
  Set.foldr
    (\re' nth ->
       Set.union (allDerivs deriv sigma re') nth)
    Set.empty
    (allNthDerivs (n-1) deriv sigma re)

derivSizes :: Derivative -> Alphabet -> Set Regex -> Set Int
derivSizes d sigma res =
  Set.map R.size $ Set.unions $ Set.map (allDerivs d sigma) res
