{-|
Module      : Grid
Description : d-dimentional cubic lattices

Copyright   : Thodoris Papakonstantinou, 2016
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX

Functions defining d-dimentional cubic lattices by giving adjacent vertices and edges of vertex
following conventional vertex labeling.

- L: linear size
- d: dimensionality (2: square, 3: cubic, >3: hypercubic)
 -}

module Data.Grid
    ( CubicLattice
    , pbcCubicLattice
    , pbcNeighbors
    ) where

import Data.Lattice
import           Control.Concurrent ()


data Direction = Forward | Backward deriving (Eq, Ord, Show, Read, Bounded, Enum)

data CubicLattice = CubicLattice
    { _l :: !L -- ^ Linear size of lattice
    , _d :: !D -- ^ Dimensionality
    , _n :: !Natural -- ^ Number of Vertices
    , _vertices :: ![Vertex] -- ^ List of Vertices
    , _edges :: ![Edge] -- ^ List of Edges
    --  Given a Vertex get List of neighboring Vertices
    , _neighbors :: Vertex -> [Vertex] 
    --  Given a Vertex get List of adjacent Edges
    , _adjacentEdges :: Vertex -> [Edge]
    } 

instance Show CubicLattice where 
  show a = show $ "Lattice: { \n" ++
                  " L : " ++ show (_l a) ++ "\n" ++
                  " D : " ++ show (_d a) ++ "\n" ++
                  " numVertices : " ++ show (_n a) ++ "\n" ++
                  " numEdges : " ++ show (length (_edges a))

instance Eq CubicLattice where 
   (==) a b = (_l a == _l b) &&
              (_d a == _d b)

instance Lattice CubicLattice where
   edges l = _edges l
   vertices l = _vertices l 
   n l = _n l
   neighbors l v = (_neighbors l) v
   adjacentEdges l v = (_adjacentEdges l) v

gridN :: L -> D -> Natural
gridN l d = l ^ d

gridVertices :: L -> D -> [Vertex]
gridVertices l d = [1 .. (fromEnum l ^ fromEnum d)]

-- | Returns the next vertex of v in the d dimension for a grid of side l
pbcNeighbor :: Vertex -> L -> D -> Direction -> Vertex
pbcNeighbor v l' d' r  | r == Forward =
                      if not $! isBoundary v l d
                        then v + innerOffset
                        else v + pbcOffset
                    | r == Backward =
                      if not $ isBoundary (v - innerOffset) l d
                        then v - innerOffset
                        else v - pbcOffset
                    where innerOffset = l^(d - 1)
                          pbcOffset = - l^d + l^(d - 1)
                          isBoundary v l d = (l^d) - (l^(d - 1)) - mod (v - 1) (l^d) <= 0
                          l = fromEnum l'
                          d = fromEnum d'

-- | Given vertex returns list of nearest neighboring vertices on a Toroidal Boundary Conditions (pbc) grid
pbcNeighbors :: L -> D -> Vertex -> [Vertex]
pbcNeighbors l d v = (\r d -> pbcNeighbor v l d r) 
  <$> [Forward,Backward] <*> [1 .. d]

-- | Returns tuple (edge) giving forward and backward vertices of given vertex on a Toroidal Boundary Conditions (pbc) grid
pbcAdjacentEdges :: L -> D -> Vertex -> [Edge]
pbcAdjacentEdges l d v = (\r d -> 
  case r of Forward ->  (v, pbcNeighbor v l d r)
            Backward -> (pbcNeighbor v l d r , v)
  ) 
  <$> [Forward,Backward] <*> [1 .. d]

-- | List of edges of grid with periodic boundary conditions
pbcEdges :: L -> D -> [Edge]
pbcEdges l d = (\v j-> (v, pbcNeighbor v l j Forward)) <$> gridVertices l d <*> [1 .. d]

pbcCubicLattice :: L -> D -> CubicLattice
pbcCubicLattice l d = CubicLattice
  { _l = l
  , _d = d
  , _n = gridN l d
  , _vertices = gridVertices l d
  , _edges = pbcEdges l d
  , _neighbors = pbcNeighbors l d 
  , _adjacentEdges = pbcAdjacentEdges l d 
  }
