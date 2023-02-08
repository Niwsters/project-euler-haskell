module Primes (test, primes) where

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
