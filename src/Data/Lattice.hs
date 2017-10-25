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
    , reverseEdge
    , Lattice (..)
    , Graph (..)
    , mapEdgeIndx
    , edgeMap
    ) where

import Data.Natural
import qualified Data.Map.Strict as M

import Data.Graph

class Graph l => Lattice l where 
  size :: l -> Natural
  numEdges :: l -> Natural
  forwardEdges :: l -> Vertex -> [Edge] -- ^ for traversing once the edges by folding onto vertices
  edgeIx :: l -> Edge -> Maybe Int

edgeMap :: Lattice l => l -> M.Map Edge Int
edgeMap l = M.fromList (zip (edges l) [1 .. fromIntegral $ numEdges l]) :: M.Map Edge Int

mapEdgeIndx :: Lattice l => l -> Edge -> Maybe Int
mapEdgeIndx l e = M.lookup e $ edgeMap l
