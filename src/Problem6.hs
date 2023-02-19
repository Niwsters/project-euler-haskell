module Problem6 (result) where

numbers = [1..100]
square x = x*x
squares numbers = map square numbers
sumOfSquares = sum (squares numbers)
squareOfSums = square (sum numbers)

result = squareOfSums - sumOfSquares
