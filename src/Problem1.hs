module Problem1 (
  result
) where

naturals = [1..]
selection = take 999 naturals
multiples = filter f selection
  where f n = mod n 3 == 0 || mod n 5 == 0
result = sum multiples
