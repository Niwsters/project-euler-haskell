module Problem7 (result) where

import Primes (primes)

result = last (take 10001 primes)
