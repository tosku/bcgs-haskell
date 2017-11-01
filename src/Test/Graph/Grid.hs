{-# LANGUAGE TemplateHaskell #-}
module Test.Graph.Grid where

import Language.Haskell.TH
import Data.Bifunctor
import Data.Graph.Grid
import Data.List
import Data.List.Unique
import Test.Test

fastTests :: [Test]
fastTests = [ test2dpbc1
            , test2dpbc2
            , test3dpbc1
            , test3dpbc2
            , test4dpbc1
            , test2dedges
            , testforwards
            , vertexToCVertexToVertex
            , testEdge
            ]

test2dpbc1 :: Test
test2dpbc1 = do
  let name = "Neighbors of 1 in a square"
      neigh1 = [2,3]
      out = sortUniq $ neighbors (PBCSquareLattice (2 :: L)  (2 :: D)) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show neigh1) (show out)

test2dpbc2 :: Test
test2dpbc2 = do
  let name = "Neighbors of 1 in L=4 D=2"
      neigh1 = [2,4,5,13]
      out = sortUniq $ neighbors (PBCSquareLattice  (4 :: L)  (2 :: D)) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show neigh1) (show out)

test3dpbc1 :: Test
test3dpbc1 = do
  let name = "Neighbors of 1 in L=2 D=3"
      neigh1 = [2,3,5]
      out = sortUniq $ neighbors (PBCSquareLattice  (2 :: L)  (3 :: D) )(1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show neigh1) (show out)


test3dpbc2 :: Test
test3dpbc2 = do
  let name = "Neighbors of 1 in L=4 D=3"
      neigh1 = [2,4, 5,13, 17,49]
      out = sortUniq $ neighbors (PBCSquareLattice  (4 :: L)  (3 :: D)) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (,) (show neigh1) (show out)

test4dpbc1 :: Test
test4dpbc1 = do
  let name = "Neighbors of 1 in L=2 D=4"
      neigh1 = [2,3,5,9]
      out = sortUniq $
            neighbors (PBCSquareLattice  (2 :: L) (4 :: D)) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name "passed!"
    False -> testFailed name $ (bimap <$> id <*> id) show (neigh1, out)

test2dedges :: Test
test2dedges = do
  let name = "Edges of pbcsql L=3 D=2"
      expe = [(1,2),(1,4),(2,3),(2,5),(3,1),(3,6),(4,5),(4,7),(5,6),(5,8),(6,4),(6,9),(7,8),(7,1),(8,9),(8,2),(9,7),(9,3)]
      out = map toTuple $ edges $ (PBCSquareLattice  (3 :: L) (2 :: D))
  case out == expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (bimap <$> id <*> id) show (expe, out)


testforwards :: Test
testforwards = do
  let name = "Edges of pbcsql L=3 D=2"
      lat  = (PBCSquareLattice  (3 :: L) (2 :: D))
      expe = [(1,2),(1,4),(2,3),(2,5),(3,1),(3,6),(4,5),(4,7),(5,6),(5,8),(6,4),(6,9),(7,8),(7,1),(8,9),(8,2),(9,7),(9,3)]
      out = foldl (\ac v -> foldl (\ac e -> e:ac) ac (map toTuple $ outEdges lat v)) [] (vertices lat)
  case all id (map (\e -> elem e expe) out) of
    True -> testPassed name "passed!"
    False -> testFailed name $ (bimap <$> id <*> id) show (expe, out)

vertexToCVertexToVertex :: Test
vertexToCVertexToVertex = do
  let name = "Turn vertex to cartesian vertex and back for PBCSquare lattice"
      l    = (3 :: L)
      d    = (3 :: D)
      vs = vertices (PBCSquareLattice l d)
      cvs = map (vertexToCVertex l d) vs 
      vs' = map (cVertexToVertex l d) cvs
  case vs == vs' of
    True -> testPassed name $ "passed!"
    False -> testFailed name $ (bimap <$> id <*> id) (show . take 10) (vs, vs')

testEdge :: Test
testEdge = do
  let name = "Edges to ids"
      l    = (40 :: L)
      d    = (3 :: D)
      lattice = (PBCSquareLattice l d)
      es = edges lattice
      eids = map (pbcEdgeIx l d) es
      expe :: [Maybe Int]
      expe = map pure [1..length es]
  case eids == expe of
    True -> testPassed name "passed!"
    False -> testFailed name $ (bimap <$> id <*> id) show (expe ,eids)

