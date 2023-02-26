module Problem14 (result) where
import Data.List
import Data.Ord

collatz n = go [] n
  where
    next n
      | even n = div n 2
      | odd n  = 3*n+1
    go sequence n
      | n == 1 = sequence
      | otherwise = go (sequence ++ [n]) (next n)

collatzNumbers = map collatz [1..(1000000-1)]

result = maximumBy (comparing length) collatzNumbers
