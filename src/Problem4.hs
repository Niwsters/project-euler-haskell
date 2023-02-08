module Problem4 (result) where

-- Thanks to https://projecteuler.net/thread=4;page=2#2655

isPalindrome n = result
  where
    str = show n
    len = length str
    a = take (div len 2) str
    b = take (div len 2) (reverse str)
    result = a == b

products = [x * y | x <- [100..999], y <- [100..999]]
palindromes = filter isPalindrome products
result = maximum palindromes
