module Problem10 (result) where
import Primes (isPrime)

primes = filter isPrime ([2] ++ [3,5..1999999])

result = sum primes
