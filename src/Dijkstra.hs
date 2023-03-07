module Dijkstra where
import Data.List
import Data.Function
import qualified Data.Map as Map

data Distance = Distance Integer | Infinity deriving (Eq, Show)

instance Ord Distance where
  Distance a < Infinity = True
  Distance a <= Infinity = True

a :: Distance
a = Infinity
b :: Distance
b = Distance 3

data Edge = Edge
  { from :: Integer,
    to :: Integer,
    distance :: Integer } deriving (Show)

nodes = [0..8]

edges = [
  Edge 0 1 4,
  Edge 0 7 8,
  Edge 1 2 8,
  Edge 1 7 11,
  Edge 2 3 7,
  Edge 2 5 4,
  Edge 2 8 2,
  Edge 3 4 9,
  Edge 3 5 14,
  Edge 4 5 10,
  Edge 5 6 2,
  Edge 6 7 1,
  Edge 6 8 6,
  Edge 7 8 1]

dijkstra nodes edges = c
  where
    -- 1. Mark your selected initial node with a current distance of 0
    -- and the rest with infinity.
    distances = Map.fromList (zip [0..] (Distance 0 : replicate (length nodes - 1) Infinity))

    -- Set the non-visited node with the smallest current distance as the current node C
    c = minimumBy (compare `on` snd) (Map.toList distances)

test = dijkstra nodes edges
