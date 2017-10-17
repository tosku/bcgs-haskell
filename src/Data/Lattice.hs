{-|
Module      : Lattice
Description : Class definitions

Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Class providing functions needed for local and global access to a Generic Lattice / Graph
 -}

module Data.Lattice
    ( Natural
    , Vertex
    , Edge (..)
    , fromTuple
    , toTuple
    , Lattice (..)
    ) where

import Data.Traversable
import Data.Natural

type Vertex = Int

data Edge = Edge Vertex Vertex 

from :: Edge -> Vertex
from (Edge s t) = s

to :: Edge -> Vertex
to (Edge s t) = t

fromTuple :: (Vertex, Vertex) -> Edge
fromTuple (s,t) = Edge s t

toTuple :: Edge -> (Vertex, Vertex)
toTuple (Edge s t) = (s,t)

instance Show Edge where
 show (Edge s t) = "(" ++ show s ++ "," ++ show t ++ ")"

instance Eq Edge where
  a == b = (from a == from b && to a == to b)|| 
           (from a == to b && from a == to b)

class Lattice l where 
  size :: l -> Natural
  vertices :: l -> [Vertex]
  edges :: l -> [Edge]
  numEdges :: l -> Natural
  edgeIx :: l -> Edge -> Maybe Int
  -- ixEdge :: l -> Int -> Maybe Edge
  neighbors :: l -> Vertex -> [Vertex] 
  adjacentEdges :: l -> Vertex -> [Edge] 
