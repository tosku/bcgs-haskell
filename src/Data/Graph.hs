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
    , Vertex (..)
    , Edge (..)
    , Graph (..)
    , fromTuple
    , toTuple
    , reverseEdge
    , reverseEdges
    , getReverseNeighbors
    , reverseGraph
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


import Data.List
import Data.Maybe
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

reverseEdges :: Graph -> [Edge]
reverseEdges g = map reverseEdge $ edges g

numVertices :: Graph -> Int
numVertices g = length $ vertices g
numEdges :: Graph -> Int
numEdges g = length $ edges g

-- | Graph definition both directed and undirected
data Graph = Graph { vertices :: [Vertex]
  -- | edges should be unique even if graph is undirected
                   , edges :: [Edge]
  -- | out vertices for directed and all neighbors for undirected graphs
                   , neighbors :: Vertex -> [Vertex]  
  -- | traverse all edges uniquely by traversing on the vertices 
                   , outEdges :: Vertex -> [Edge] 
  -- | gives the position of the edge to the edges list
                   , edgeIndex :: Edge -> Maybe Int
                   }

instance Eq Graph where
  (==) g1 g2 = (sort (vertices g1) == sort (vertices g2))
               && (sort (edges g1) == sort (edges g2))

instance Show Graph where
  show g = "vertices: " ++ show (vertices g) ++ "\n" ++
            "edges: " ++ show (edges g) ++ "\n"

outVertices g v = map from $ filter (\e -> from e == v) (edges g)

edgesFromNeighbors :: Graph -> [Edge]
edgesFromNeighbors g = 
  foldl (\ac v -> 
    ac ++ map (\n -> Edge v n) (neighbors g v)
        ) [] $ vertices g

adjacentEdges :: Graph -> Vertex -> [Edge]
adjacentEdges g v = map (\n -> Edge v n) $ neighbors g v

edgeMap :: Graph -> M.Map Edge Int
edgeMap g = M.fromList (zip (edges g) [1..]) :: M.Map Edge Int

mapEdgeIndx :: Graph -> Edge -> Maybe Int
mapEdgeIndx g e = M.lookup e $ edgeMap g

adjacencyMap :: Graph -> IM.IntMap [Vertex]
adjacencyMap g = IM.fromList $ map (\v -> (v, (neighbors g v))) vs
                 where vs = vertices g

getReverseNeighbors :: [Vertex] -> [Edge] -> IM.IntMap [Vertex]
getReverseNeighbors vs es = IM.fromList $ zip vs (map (\v -> map from (filter (\re -> to re == v) es)) vs)
{-# getReverseNeighbors #-}

reverseGraph :: Graph -> Graph
reverseGraph g = Graph { vertices = vertices g
                       , edges = reverseEdges g
                       , neighbors = (\v -> fromJust (IM.lookup v (getReverseNeighbors (vertices g) (edges g))))
                       , outEdges = (\v -> map (\n -> Edge v n) ((neighbors (reverseGraph g)) v))
                       , edgeIndex = mapEdgeIndx $ reverseGraph g
                       }
