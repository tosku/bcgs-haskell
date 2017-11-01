{-|
Module      : Graph
Description : Class definitions

Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Graph TypeClass definitions
 -}

module Data.Graph
    ( Natural
    , Vertex
    , Edge (..)
    , Graph (..)
    , fromTuple
    , toTuple
    , reverseEdge
    , reverseEdges
    , adjacentEdges
    , edgesFromNeighbors
    , adjacencyMap
    , mapEdgeIndx
    , edgeMap
    , from
    , to
    , numVertices
    , numEdges
    , outVertices
    ) where


import Data.Natural
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM

type Vertex = Int

data Edge = Edge Vertex Vertex 
  deriving (Ord)

instance Show Edge where
 show (Edge s t) = "[" ++ show s ++ "->" ++ show t ++ "]"

instance Eq Edge where
  a == b = from a == from b && to a == to b

from :: Edge -> Vertex
from (Edge s t) = s

to :: Edge -> Vertex
to (Edge s t) = t

fromTuple :: (Vertex, Vertex) -> Edge
fromTuple (s,t) = Edge s t

toTuple :: Edge -> (Vertex, Vertex)
toTuple (Edge s t) = (s,t)

reverseEdge :: Edge -> Edge
reverseEdge (Edge s t) = Edge t s

reverseEdges :: Graph g => g -> [Edge]
reverseEdges g = map reverseEdge $ edges g

numVertices :: Graph g => g -> Int
numVertices g = length $ vertices g
numEdges :: Graph g => g -> Int
numEdges g = length $ edges g

-- | Graph definition both directed and undirected
class Eq g => Graph g where 
  vertices :: g -> [Vertex]
  -- | edges should be unique even if graph is undirected
  edges :: g -> [Edge]
  -- | out vertices for directed and all neighbors for undirected graphs
  neighbors :: g -> Vertex -> [Vertex]  
  -- | traverse all edges uniquely by traversing on the vertices 
  outEdges :: g -> Vertex -> [Edge] 
  -- | finds the index
  edgeIndex :: g -> Edge -> Maybe Int

outVertices g v = map from $ filter (\e -> from e == v) (edges g)

edgesFromNeighbors :: Graph g => g -> [Edge]
edgesFromNeighbors g = 
  foldl (\ac v -> 
    ac ++ map (\n -> Edge v n) (neighbors g v)
        ) [] $ vertices g

adjacentEdges :: Graph g => g -> Vertex -> [Edge]
adjacentEdges g v = map (\n -> Edge v n) $ neighbors g v

edgeMap :: Graph l => l -> M.Map Edge Int
edgeMap l = M.fromList (zip (edges l) [1..]) :: M.Map Edge Int

mapEdgeIndx :: Graph l => l -> Edge -> Maybe Int
mapEdgeIndx l e = M.lookup e $ edgeMap l

adjacencyMap :: Graph l => l -> IM.IntMap [Vertex]
adjacencyMap l = IM.fromList $ map (\v -> (v, (neighbors l v))) vs
                 where vs = vertices l
