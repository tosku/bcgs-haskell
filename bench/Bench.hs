module Main where

import Criterion.Main
import qualified Data.Vector.Unboxed                   as V

import qualified Data.PRNG as RNG
import qualified Data.PRNG.MTRNG as MT

import Data.Grid
import Data.Lattice
import Data.BlumeCapel

main = do
  let l = 60
      d = 3
      s = 134
      r = 1.9
      ureal = RBBC (UnimodalDisorder s r) (PBCSquareLattice l d)
      breal = RBBC (BimodalDisorder s r) (PBCSquareLattice l d)
      nume = fromIntegral $ numEdges (lattice $ ureal)
  writeFile "bench/bench.out" $ show (V.sum (interactions $ ureal) / nume) ++ "\n" ++ 
    show (V.take 10 (interactions breal)) ++ "\n"
  print $ show (V.take 19 (interactions ureal)) ++ "\n"
  defaultMain [ 
                {-bgroup "Blume-Capel realization" [ bench "20^3 dichotomous" $ nf (\s -> V.sum (breal (20 :: L) (3 :: D) s r) ) seed-}
                                                 {-{-, bench "20^3 normal" $ nf (\s -> V.sum (ureal (20 :: L) (3 :: D) s r) ) seed-}-}
                                                 {-{-, bench "60^3 dichotomous" $ nf (\s -> V.sum (breal (60 :: L) (3 :: D) s) ) s-}-}
                                                 {-{-, bench "60^3 normal" $ nf (\s -> V.sum (ureal (60 :: L) (3 :: D) s) ) s-}-}
                    {-]-}
              ]
