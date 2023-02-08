module Primes2 (test) where

primesTo :: Integer -> Integer
primesTo goal = repeat goal
  where
    repeat :: Integer -> Integer
    repeat 0 = repeat (goal+1)
    repeat n | n == goal = n
             | otherwise = repeat (n+1)

test = primesTo 20
