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
    ( Natural
    , L
    , D
    , Vertex
    , Edge
    , fromTuple
    , toTuple
    , Lattice (..)
    , vertexToCVertex
    , cVertexToVertex
    , PBCSquareLattice (..)
    ) where

import qualified Data.Vector as V

import Data.Lattice

type L = Natural
type D = Natural

type CVertex = [Vertex] -- ^ Representation of a Lattice Vertex as Cartesian graph product
data CEdge = CEdge CVertex CVertex -- ^ Cartesian representation of a Lattice Vertex

data Direction = Forward | Backward deriving (Eq, Ord, Show, Read, Bounded, Enum)

class SquareLattice l where
    linearSize :: l -> L -- ^ Linear size of lattice
    dimension :: l -> D -- ^ Dimensionality

-- | A PBCSquareLattice is the Cartesian product of a cycle graph of length L
-- (C_L) => (C_L)▢^d
data PBCSquareLattice = PBCSquareLattice L D

instance Lattice PBCSquareLattice where
  size (PBCSquareLattice l d) = gridN l d
  neighbors (PBCSquareLattice l d) = pbcNeighbors l d
  adjacentEdges (PBCSquareLattice l d) = pbcAdjacentEdges l d
  vertices (PBCSquareLattice l d) = gridVertices l d
  edges (PBCSquareLattice l d) = pbcEdges l d
  numEdges (PBCSquareLattice l d) = d * (gridN l d)
  edgeIx (PBCSquareLattice l d) = pbcEdgeIx l d
  {-ixEdge (PBCSquareLattice l d) = pbcIxEdges l d-}
  
instance SquareLattice PBCSquareLattice where
  linearSize (PBCSquareLattice l d) = l
  dimension (PBCSquareLattice l d) = d

instance Eq PBCSquareLattice where 
  (==) (PBCSquareLattice la da) (PBCSquareLattice lb db) = 
    la == la && da == db        

instance Show PBCSquareLattice where 
  show a = "Lattice: { \n" ++
                  " L : " ++ show (linearSize a) ++ "\n" ++
                  " D : " ++ show (dimension a) ++ "\n" ++
                  " numVertices : " ++ show (size a) ++ "\n" ++
                  " numEdges : " ++ show (length (edges a))



gridN :: L -> D -> Natural
gridN l d = l ^ d

gridVertices :: L -> D -> [Vertex]
gridVertices l d = [1 .. (fromEnum l ^ fromEnum d)]

-- | Returns the next vertex of v in the d dimension for a grid of side l
pbcNeighbor :: Vertex -> L -> D -> Direction -> Vertex
pbcNeighbor v l d r  | r == Forward =
                      if not $! isBoundary v l d
                        then v + innerOffset l d
                        else v + pbcOffset l d 
                    | r == Backward =
                      if not $ isBoundary (v - innerOffset l d) l d
                        then v - innerOffset l d
                        else v - pbcOffset l d

innerOffset :: L -> D -> Vertex
innerOffset l' d' = l^(d - 1)
  where l = fromEnum l'
        d = fromEnum d'

pbcOffset :: L -> D -> Vertex
pbcOffset l' d' = - l^d + l^(d - 1)
  where l = fromEnum l'
        d = fromEnum d'

isBoundary :: Vertex -> L -> D -> Bool
isBoundary v l' d' = (l^d) - (l^(d - 1)) - mod (v - 1) (l^d) <= 0
  where l = fromEnum l'
        d = fromEnum d'

-- | Given vertex returns list of nearest neighboring vertices on a Toroidal Boundary Conditions (pbc) grid
pbcNeighbors :: L -> D -> Vertex -> [Vertex]
pbcNeighbors l d v = (\r d'-> pbcNeighbor v l d' r) 
  <$> [Forward,Backward] <*> [1 .. d]

-- | Given a Vertex returns a tuple of the Cartesian product of a L sized Cycle graph
vertexToCVertex :: L -> D -> Vertex -> CVertex
vertexToCVertex l' d' v = do
  let cix l n i = (mod (div (n-1) (l^(i-1))) l) + 1
      out = map (cix l v) [1 .. d]
  out
  where l = fromEnum l'
        d = fromEnum d'

-- | The reverse function of vertexToCVertex
cVertexToVertex :: L -> D -> CVertex -> Vertex
cVertexToVertex l' d' cv = do
  (foldr (\t@(i,x)-> (+) ((x-1) * (l^(i-1)))) 0 $ zip [1 .. d] cv) + 1
  where l = fromEnum l'
        d = fromEnum d'

-- | Gives Forward vertex in a cycle graph of length L
forwardVertexInCycle :: L -> Vertex -> Vertex
forwardVertexInCycle l' v
  | v == l = 1
  | otherwise = v + 1
  where l = fromEnum l'

-- | Gives Forward vertex in a cycle graph of length L
backwardVertexInCycle :: L -> Vertex -> Vertex
backwardVertexInCycle l' v
  | v == 1 = l
  | otherwise = v - 1
  where l = fromEnum l'

-- | Given two edges returns if they belong to the lattice
isEdgeInCycle :: L -> Edge -> Bool
isEdgeInCycle l' (Edge a b)
  | a == b - 1 = True
  | a == b + 1 = True
  | a == l && b == 1 = True
  | b == l && a == 1 = True
  | otherwise = False
  where l = fromEnum l'


-- | Returns tuple (edge) giving forward and backward vertices of given vertex on a Toroidal Boundary Conditions (pbc) grid
pbcAdjacentEdges :: L -> D -> Vertex -> [Edge]
pbcAdjacentEdges l d v = (\r d -> 
  case r of Forward ->  Edge v (pbcNeighbor v l d r)
            Backward -> Edge (pbcNeighbor v l d r) v
  ) 
  <$> [Forward,Backward] <*> [1 .. d]

-- | List of edges of grid with periodic boundary conditions
pbcEdges :: L -> D -> [Edge]
pbcEdges l d = (\v j-> Edge v (pbcNeighbor v l j Forward)) <$> gridVertices l d <*> [1 .. d]

-- | Index of edge of a grid with periodic boundary conditions
pbcEdgeIx :: L -> D -> Edge -> Maybe Int
pbcEdgeIx l d e = do
  let Edge s t = e
      a = vertexToCVertex l d s
      b = vertexToCVertex l d t
      (((a',b'),di),dist) = diff (CEdge a b)
  if dist == 1 then
    if forwardVertexInCycle l a' == b' then
      Just $ ((s-1)*d') + di
      -- Just di
      else
      Just $ ((t-1)*d') + di
      -- Just di
  else
    Nothing
  where
    d' = fromEnum d
    step (((a',b'),di'), ds) ((s,t),di)
      | s == t = (((a',b'),di'), ds)
      | s /= t = (((s,t),di), ds+1)
    diff :: CEdge -> (((Vertex,Vertex),Int),Int)
    diff (CEdge a b) = foldl step (((0,0),0),0) $ zip (zip a b) [1..d']

