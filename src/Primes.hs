module Primes (test, isPrime, primes, primeFactors, lowestPrime) where

import Data.List (find)

-- Cheers to https://stackoverflow.com/a/11769856

isPrime n = go 2
  where
    go d
      | d*d > n      = True
      | rem n d == 0 = False
      | otherwise    = go (d+1)

primes = filter isPrime [2 .. ]

assertEqual a b
  | a == b    = "Assertion passed"
  | otherwise = "Expected " ++ (show a) ++ " and " ++ (show b) ++ " to be equal"

test = assertEqual (last (take 100 primes)) 541

lowestPrime :: Integer -> Integer
lowestPrime x = case find (\n -> mod x n == 0) primes of
  Just n -> n
  Nothing -> 1

primeFactors n = go []
  where
    go factors
      | div n (product factors) == 1 = factors
      | otherwise = go (prime : factors)
      where
        prime = lowestPrime n
