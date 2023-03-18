module Problem20 (result) where
import Data.List.Split (splitOn)

fact n = product [2..n]
parse :: String -> Integer
parse = read
result = (sum . map parse . filter (/="") . splitOn "" . show . fact) 100
