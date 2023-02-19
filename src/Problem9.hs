module Problem9 (result) where

import Data.List

-- a+b+c = 1000
-- => c = 1000 - a - b
-- => c*c = 1000^2 - 1000a - 1000b - 1000a + a^2 + a*b - 1000b + a*b + b*b
--        = 1e6 - 2000a - 2000b + 2*a*b + a*a + b*b
-- c*c = a*a + b*b
-- 1e6 - 2000a - 2000b + 2*a*b + a*a + b*b = a*a + b*b
-- => 1e6 - 2000a - 2000b + 2*a*b = 0
-- => 2*a*b - 2000b = 2000a - 1e6
-- => b(2*a - 2000) = 2000a - 1e6
-- => b = (2000a - 1e6) / (2*a - 2000)

isNatural x = x == fromInteger (round x) && x > 0

a = [1..998]
ab = natural
  where
    isBNatural (a,b) = isNatural b
    possible = map (\a -> (a, (2000*a - 1e6) / (2*a - 2000))) a
    natural  = filter isBNatural possible
triplets = map (\(a, b) -> (a, b, 1000 - a - b)) ab

-- Alternate solution:
-- combinations =
--   [(a, b, (1000 - a - b)) | a <- [1..998], b <- [1..998]]
--   :: [(Integer, Integer, Integer)]
-- 
-- isTriplet (a,b,c) = a*a + b*b == c*c
-- triplets = filter isTriplet combinations

first (x:xs) = x

prod (a,b,c) = a*b*c

result = prod (first triplets)
