module Primes (test, isPrime, primes, primeFactors, primeFactorMap, lowestPrime) where

import Data.List (find)

-- Cheers to https://stackoverflow.com/a/11769856

isPrime n = go 2
  where
    go d
      | d*d > n      = True
      | rem n d == 0 = False
      | otherwise    = go (d+1)

primes = filter isPrime [2 .. ]

testPrimes = assertEqual (last (take 100 primes)) 541

lowestPrime :: Integer -> Integer
lowestPrime x = case find (\n -> mod x n == 0) primes of
  Just n -> n
  Nothing -> 1

primeFactorMap n = go [] (primeFactors n) 
  where
    go m [] = m
    go m (x:factors) = go ((x,occurences) : m) (filter (/=x) factors)
      where
        occurences = (length (filter (==x) factors) + 1)

primeFactors n = go [] n
  where
    go factors n
      | n == 1    = factors
      | otherwise = go (prime : factors) (div n prime) 
      where
        prime = lowestPrime n

testPrimeFactors = assertEqual (primeFactors 28) [2,2,7]

assertEqual a b
  | a == b    = "Assertion passed"
  | otherwise = "Expected " ++ (show a) ++ " and " ++ (show b) ++ " to be equal"

test = [
  testPrimes,
  testPrimeFactors]
