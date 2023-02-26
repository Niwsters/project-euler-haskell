module Problem12 (result) where
import Data.List

triangleNumbers = map triangleNumber [1..]
  where
    triangleNumber n = sum [1..n]

divisors :: Integer -> [Integer]
divisors n = go [] 1
  where
    go divisors i
      | i*i > n = divisors
      | mod n i == 0 = go (divisors ++ [i, (div n i)]) (i+1)
      | otherwise = go divisors (i+1)

result = find (\x -> length (divisors x) > 500) triangleNumbers
