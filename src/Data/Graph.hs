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
    , fromTuple
    , toTuple
    , reverseEdge
    , DiGraph (..)
    , Graph (..)
    ) where

import Data.Natural
import qualified Data.IntMap.Strict as IM

type Vertex = Int

-- | Directed Edge
data Edge = Edge Vertex Vertex 
  deriving (Ord)

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

instance Show Edge where
 show (Edge s t) = "(" ++ show s ++ "," ++ show t ++ ")"

-- | Graph definition both directed and undirected
class Graph g where 
  vertices :: g -> [Vertex]
  edges :: g -> [Edge]
  neighbors :: g -> Vertex -> [Vertex]  -- ^ outVertices for directed graphs
  adjacentEdges :: g -> Vertex -> [Edge] -- ^ outEdges for directed graphs

class Graph g => DiGraph g where
  inVertices :: g -> Vertex -> [Vertex] 
  inEdges :: g -> Vertex -> [Edge] 


--bfs :: Graph g => g -> Vertex -> ()
