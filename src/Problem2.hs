module Problem2 (result) where

fib :: Integer -> Integer
fib n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

naturals = take 1000 [1..]
fibs = map fib naturals

numbers = filter f (filter (<4000000) fibs)
  where f n = mod n 2 == 0

result = sum numbers
