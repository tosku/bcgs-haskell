{-|
Module      : Lattice
Description : Class definitions

Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Class providing functions needed for local and global access of a lattice
 -}

module Data.Lattice
    ( Vertex
    , Edge
    , L
    , D
    , Lattice
    , n
    , edges
    , vertices
    , neighbors
    , adjacentEdges
    , Natural
    ) where

import Data.Traversable
import Data.Natural

type Vertex = Int
type Edge = (Vertex, Vertex)
type L = Natural
type D = Natural

class Lattice l where 
  neighbors :: l -> Vertex -> [Vertex] 
  adjacentEdges :: l -> Vertex -> [Edge] 
  n :: l -> Natural
  vertices :: l -> [Vertex]
  edges :: l -> [Edge]
  -- traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
  -- sequenceA :: Applicative f => t (f a) -> f (t a) Source #
