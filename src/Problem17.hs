module Problem17 (result) where
import Data.Array
import Data.List.Split

lowStrs = listArray (0,19) [
  "",
  "one",
  "two",
  "three",
  "four",
  "five",
  "six",
  "seven",
  "eight",
  "nine",
  "ten",
  "eleven",
  "twelve",
  "thirteen",
  "fourteen",
  "fifteen",
  "sixteen",
  "seventeen",
  "eighteen",
  "nineteen"]

lows n = lowStrs ! n

tenStrs = listArray (2,9) [
  "twenty",
  "thirty",
  "forty",
  "fifty",
  "sixty",
  "seventy",
  "eighty",
  "ninety"]

tens n
  | n >= 20 = tenStrs ! div' ++ lows'
  | otherwise = str n
  where
    div' = div n 10
    rem' = rem n (10*div')
    lows' | rem' > 0 = " " ++ lows rem'
          | otherwise = ""

hundreds n 
  | div' > 0 = lows div' ++ " hundred" ++ and
  | otherwise = ""
  where
    div' = div n 100
    rem' = rem n (100*div')
    and | rem' == 0 = ""
        | otherwise = " and " ++ tens rem'

thousands n
  | n > 999 = thousands' ++ hundreds rem'
  | otherwise = ""
  where
    thousands' | div' > 0  = lows div' ++ " thousand"
               | otherwise = ""
    div' = div n 1000
    rem' = rem n (1000 * div')

str :: Integer -> String
str n
  | n >= 1000 = thousands n
  | n >= 100  = hundreds n
  | n >= 20   = tens n
  | otherwise = lows n

one = "one"
two = "two"
three = "three"
four = "four"
five = "five"
six = "six"
seven = "seven"

result = (length . concat . concatMap (splitOn " " . str)) [1..1000]
