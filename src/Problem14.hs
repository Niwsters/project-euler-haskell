module Problem14 (result) where
import Data.Array
import Data.List
import Data.Ord
import Debug.Trace

collatzNumbers n = numbers
  where
    numbers = listArray(1,n) $ 0 : map go [2..n]
    go x
      | y <= n = 1 + numbers ! y
      | otherwise = 1 + go y
      where
        y | even x    = div x 2
          | otherwise = 3*x+1

result = maximumBy (comparing snd) (assocs (collatzNumbers 1000000))
