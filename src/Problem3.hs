module Problem3 (result) where
import Primes (primes)
import Data.List (find)

number = 600851475143

nextPrime :: Integer -> Integer
nextPrime x = case find (\n -> mod x n == 0) primes of
  Just n -> n
  Nothing -> 1

largestPrime 1 largest = largest
largestPrime n largest = largestPrime dividedNumber prime
  where
    prime = nextPrime n
    dividedNumber = div n prime

result = largestPrime number 1
