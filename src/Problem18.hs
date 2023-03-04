module Problem18 (result) where
import Data.List.Split
import Data.Array

testInput = "3\n7 4\n2 4 6\n8 5 9 3"
parse :: String -> Integer
parse = read

result = listArray (1,4) (map (map parse . splitOn " ") (splitOn "\n" testInput))
