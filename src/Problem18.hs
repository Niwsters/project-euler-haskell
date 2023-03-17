module Problem18 (result) where
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import Debug.Trace
import Data.List
import Data.Bifunctor
import Data.Function
import Data.Foldable
import Dijkstra

testInput = "3\n7 4\n2 4 6\n8 5 9 3"
input = "75\n95 64\n17 47 82\n18 35 87 10\n20 04 82 47 65\n19 01 23 75 03 34\n88 02 77 73 07 63 67\n99 65 04 28 06 16 70 92\n41 41 26 56 83 40 80 70 33\n41 48 72 33 47 32 37 16 94 29\n53 71 44 65 25 43 91 52 97 51 14\n70 11 33 28 77 73 17 78 39 68 17 57\n91 71 52 38 17 14 91 43 58 50 27 29 48\n63 66 04 68 89 53 67 30 73 16 69 87 40 31\n04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"

parse :: String -> Integer
parse = read
lines' = lines input
pyramid = map (map parse . splitOn " ") lines'
go pyramid = result
  where
    as = head pyramid
    bs = pyramid !! 1
    newHead = add as bs
    result = newHead : drop 2 pyramid

add as bs = result
  where
    flat = concatMap (\(i,a) -> [(i,a + bs !! i),(i+1,a + bs !! (i+1))]) (zip [0..] as)
    cols = nub (map fst flat)
    result = map (\i -> (snd . maximum) (filter (\(i2,_) -> i2 == i) flat)) cols

solve pyramid
  | length pyramid == 1 = (maximum . head) pyramid
  | otherwise = solve pyramid'
  where
    pyramid' = go pyramid

result = solve pyramid
