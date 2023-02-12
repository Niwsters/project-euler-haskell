module Problem5 (result) where

import Primes (primeFactors)
import Data.List

factors = go (map primeFactors [1..20]) []
  where
    go [] facs = facs
    go (x:xs) facs = go xs ((facs \\ x) ++ x)

result = product factors
