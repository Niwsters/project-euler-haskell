module Problem15 (result) where

f n x
  | n == 0    = 1
  | x == 0    = 1
  | otherwise = (f n (x-1)) + (f (n-1) x)

solve limit = f limit limit

result = solve 15
