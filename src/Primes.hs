module Primes (primes) where

import Data.Array.Unboxed
 
primes :: [Int]
primes = 2 : oddprimes ()
  where
    oddprimes = (3 :) . sieve 3 [] . oddprimes
    sieve x fs (p:ps) = [i*2 + x | (i,True) <- assocs a]
                        ++ sieve (p*p) ((p,0) :
                             [(s, rem (y-q) s) | (s,y) <- fs]) ps
      where
      q = (p*p-x)`div`2
      a :: UArray Int Bool
      a = accumArray (\ b c -> False) True (1,q-1)
                     [(i,()) | (s,y) <- fs, i <- [y+s, y+s+s..q]]
