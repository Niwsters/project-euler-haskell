module Problem5 (result) where

import Primes (primeFactors)
import Data.List (union)

--factorMap :: [Integer] -> Map Integer [Integer] 
--factorMap numbers = go numbers (fromList [])
--  where
--    go [] m = m
--    go (x:xs) m = go xs newM
--      where
--        factors = primeFactors x
--        newM = insert x factors m

factors = go [1..20] []
  where
    go [] facs = facs
    go (x:xs) facs = go xs (ljoin facs (primeFactors x)) 

result = factors
