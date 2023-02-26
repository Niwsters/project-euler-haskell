module Problem3 (result) where
import Primes (primes, lowestPrime)
import Data.List (find)

number = 600851475143

largestPrime n = go n 1
  where
    go 1 largest = largest
    go n largest = go dividedNumber prime
      where
        prime = lowestPrime n
        dividedNumber = div n prime

result = largestPrime number
