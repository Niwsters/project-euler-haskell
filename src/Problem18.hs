module Problem18 (result) where
import Data.List.Split
import Data.Array
import qualified Data.Set as Set
import Debug.Trace
import Data.Bifunctor
import Data.Function
import Data.Foldable

testInput = "3\n7 4\n2 4 6\n8 5 9 3"
parse :: String -> Integer
parse = read
weights = listArray (1,10) (concatMap (map parse . splitOn " ") (splitOn "\n" testInput))

trianglesum n = sum [1..n]

n = [1..]
inf = map (\x -> (take x . drop (trianglesum (x-1))) n) n
inf' = concatMap (\(r,ns) -> map (r,) ns) (zip [1..] inf)
inf'' = map (\(r,n) -> (r+n, r+n+1)) inf'
neighbors = listArray (1,6) (take 6 inf'')
neighborWeights = listArray (1,6) (map (bimap (weights !) (weights !)) (elems neighbors))


infinity = 99999999
dist = 0 : replicate (length weights - 1) infinity
prev = replicate (length weights) False
q = [1..(length weights)]


pyramid = map (map parse . splitOn " ") (splitOn "\n" testInput)
solve []          result = result
solve (p:pyramid) result = solve pyramid result'
  where
    result' = concatMap (\n -> map (+n) p) (head pyramid)

sum' a = concatMap (\n -> map (+n) a)

go (p:pyramid) = sum' p (head pyramid) : tail pyramid

result = (maximum . concat) (go (go (go pyramid)))
