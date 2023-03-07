module Dijkstra where
import Data.List
import Data.Function
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace

data Distance = Distance Integer | Infinity deriving (Eq, Show)
Distance a +++ b = Distance (a + b)

instance Ord Distance where
  Distance a < Distance b = a < b
  Distance a < Infinity = True
  Infinity < Infinity = False
  Distance a <= Distance b = a <= b
  Distance a <= Infinity = True
  Infinity <= Infinity = False


instance Num Distance where
  Distance a + Distance b = Distance (a+b)
  Distance a + Infinity = Infinity
  Infinity + Distance a = Infinity
  Infinity + Infinity = Infinity
  Distance a * Distance b = Distance (a*b)
  abs (Distance a) = Distance (abs a)
  fromInteger = Distance
  signum (Distance a) = Distance (signum a)
  negate (Distance a) = Distance (negate a)

a :: Distance
a = Infinity
b :: Distance
b = Distance 3

data Edge = Edge
  { from :: Integer,
    to :: Integer,
    distance :: Distance } deriving (Show)

doubleEdge a b distance = [Edge a b (Distance distance), Edge b a (Distance distance)]

nodes = [0..8]

edges =
  doubleEdge 0 1 4 ++
  doubleEdge 0 7 8 ++
  doubleEdge 1 2 8 ++
  doubleEdge 1 7 11 ++
  doubleEdge 2 3 7 ++
  doubleEdge 2 5 4 ++
  doubleEdge 2 8 2 ++
  doubleEdge 3 4 9 ++
  doubleEdge 3 5 14 ++
  doubleEdge 4 5 10 ++
  doubleEdge 5 6 2 ++
  doubleEdge 6 7 1 ++
  doubleEdge 6 8 6 ++
  doubleEdge 7 8 1

nodes2 = [1..6]
edges2 = 
  doubleEdge 1 2 7 ++
  doubleEdge 1 3 9 ++
  doubleEdge 1 6 14 ++
  doubleEdge 2 3 10 ++
  doubleEdge 2 4 15 ++
  doubleEdge 3 6 2 ++
  doubleEdge 3 4 11 ++
  doubleEdge 4 5 6 ++
  doubleEdge 5 6 9

dijkstra nodes edges = go distances visited
  where
    -- 1. Mark your selected initial node with a current distance of 0
    -- and the rest with infinity.
    distances = Map.fromList (zip [(head nodes)..] (Distance 0 : replicate (length nodes - 1) Infinity))
    visited :: Set.Set Integer
    visited = Set.fromList []

    go distances visited
      | null nonVisited = distances
      | otherwise = go distances' visited'
      where
        -- 2. Set the non-visited node with the smallest current distance as the current node C
        allDistances = Map.toList distances
        nonVisited = filter (\(node,_) -> not (Set.member node visited)) allDistances
        (c,distanceC) = minimumBy (compare `on` snd) nonVisited

        -- 3. For each neighbor N of your current node C:
        neighboringEdges = filter (\edge -> from edge == c) edges
        -- add the current distance of C with the weight of the edge connecting C-N.
        -- If it's smaller than the current distance of N, set it as the new current distance of N.
        newDistance edge = distanceC + distance edge
        replaceDistance distances edge = Map.adjust (min (newDistance edge)) (to edge) distances
        distances' = foldl replaceDistance distances neighboringEdges

        -- 4. Mark th currnt node C as visited.
        visited' = trace (show distances') Set.insert c visited

test = dijkstra nodes2 edges2
