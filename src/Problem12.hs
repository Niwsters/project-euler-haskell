module Problem12 (result) where
import Data.List
import Primes

triangleNumber n = sum [1..n]

divisorCount n = result
  where
    factors = primeFactorMap n
    counts = map (\(x,y) -> y) factors
    succed = map succ counts
    result = product succed

divisors :: Integer -> [Integer]
divisors n = go [] 1
  where
    go divisors i
      | i*i > n = divisors
      | rem n i == 0 = go (divisors ++ [i, (div n i)]) (i+1)
      | otherwise = go divisors (i+1)

result = go 1 1
  where
    go i sum'
      | divisorCount sum' > 500 = sum'
      | otherwise = go (i+1) (sum'+(i+1))
