module Test.MTRNG where

import Data.List
import Test.Test
import Data.IntMap

import Data.MTRNG
import qualified Data.Vector                   as V

fastTests :: [Test]
fastTests = [ test1
            ]

test1 :: Test
test1 = do
  let name = "Check permutations contains all elements"
      ls   = [3..4]
      seed = 13
      out = 4
      expe = 4
      -- out  = numEdges $ lattice real
      -- out = energy $ BC (PBCSquareLattice (2 :: L)  (3 :: D))
  case out == expe of
    True -> testPassed name $ show ls ++ "passed!"
    False -> testFailed name $ (,) (show expe) ((show out) ++ show ls)
