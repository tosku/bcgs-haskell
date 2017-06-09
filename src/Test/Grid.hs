{-# LANGUAGE TemplateHaskell #-}
module Test.Grid where

import Language.Haskell.TH
import Control.Lens
import Data.Grid
import Data.Lattice
import Data.List
import Data.List.Unique
import Test.Test

runTests :: [Test]
runTests = [ test2dpbc1
           , test2dpbc2
           , test3dpbc1
           , test3dpbc2
           , test4dpbc1]

test2dpbc1 :: Test
test2dpbc1 = do
  let name = "Neighbors of 1 in a square"
      neigh1 = [2,3]
      out = sortUniq $ pbcNeighbors (2 :: L)  (2 :: D) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name (show out)
    False -> testFailed name $ (,) (show neigh1) (show out)

test2dpbc2 :: Test
test2dpbc2 = do
  let name = "Neighbors of 1 in L=4 D=2"
      neigh1 = [2,4,5,13]
      out = sortUniq $ pbcNeighbors (4 :: L)  (2 :: D) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name (show out)
    False -> testFailed name $ (,) (show neigh1) (show out)

test3dpbc1 :: Test
test3dpbc1 = do
  let name = "Neighbors of 1 in L=2 D=3"
      neigh1 = [2,3,5]
      out = sortUniq $ pbcNeighbors (2 :: L)  (3 :: D) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name (show out)
    False -> testFailed name $ (,) (show neigh1) (show out)


test3dpbc2 :: Test
test3dpbc2 = do
  let name = "Neighbors of 1 in L=4 D=3"
      neigh1 = [2,4, 5,13, 17,47]
      out = sortUniq $ pbcNeighbors (4 :: L)  (3 :: D) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name (show out)
    False -> testFailed name $ (,) (show neigh1) (show out)


test4dpbc1 :: Test
test4dpbc1 = do
  let name = "Neighbors of 1 in L=2 D=4"
      neigh1 = [2,3,5,9]
      out = sortUniq $
            pbcNeighbors (2 :: L) (4 :: D) (1 :: Vertex)
  case out == neigh1 of
    True -> testPassed name (show out)
    False -> testFailed name $ (bimap <$> id <*> id) show (neigh1, out)
