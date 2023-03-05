module Problem1 (result) where

multiples = filter (\n -> rem n 3 == 0 || rem n 5 == 0) [1..999]
result = sum multiples
