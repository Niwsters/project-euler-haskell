module Problem18 (result) where
import Data.List.Split
import Data.Array
import Debug.Trace

testInput = "3\n7 4\n2 4 6\n8 5 9 3"
parse :: String -> Integer
parse = read
rows = listArray (1,4) (map (map parse . splitOn " ") (splitOn "\n" testInput))

mapping (r,w) = map (r,) w
paths = listArray (1,10) (concatMap mapping (assocs rows))
arr = array (1,10) (map (\(i,(r,w)) -> (i,(r,w,(r+i,r+i+1)))) (assocs paths))

nav = go 1 []
  where
    go i q
      | i == limit = q
      | otherwise = go (i+1) q'
      where
        limit = length arr
        (r,w,(a,b)) = arr ! i
        (ra,wa,(_,_)) = arr ! a
        (rb,wb,(_,_)) = arr ! b
        q'
          | a <= limit && b <= limit = q ++ [(wa,a),(wb,b)]
          | otherwise = q

trianglesum n = sum [1..n]

n = [1..]
inf = map (\x -> (take x . drop (trianglesum (x-1))) n) n
inf' = concatMap (\(r,ns) -> map (r,) ns) (zip [1..] inf)
inf'' = map (\(r,n) -> (r+n, r+n+1)) inf'

result = take 10 inf''
