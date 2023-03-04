module Problem16 (result) where
import Data.List.Split

n :: Integer
n = 2^1000
parse :: String -> Integer
parse = read

result = (sum . map parse . filter (/="") . splitOn "" . show) n
