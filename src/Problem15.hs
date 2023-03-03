module Problem15 (result) where

row x = scanl (+) 1 (tail x)
result = iterate row (repeat 1) !! 20 !! 20
