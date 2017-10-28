{-|
Module      : GSNetwork
Description : The reduction of finding the ground state Blume-Capel realization to the max-flow of a flow graph 
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}
 {-# LANGUAGE MultiParamTypeClasses #-}
 {-# LANGUAGE FunctionalDependencies #-}
 {-# LANGUAGE FlexibleInstances #-}


module Data.BlumeCapel.GSNetwork
  ( weights
  , maxFlow
  , groundState  
  , GSFG (..)
  , Network (..)
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM

import Data.Graph
import Data.Graph.Lattice
import Data.BlumeCapel


class Lattice f => Network f where
  source :: f -> Vertex
  sink :: f -> Vertex
  -- | should respect the `edges` function's ordering: 
  -- Source ++ Realization ++ Target
  capacities :: f -> [Double]
  flows :: f -> [Double] -- ^ should also respect the `edges` function's ordering
  adjacencyMap :: f -> IM.IntMap [Vertex]

-- | Ground state flow graph from rbbc realization
data (Disorder d, Lattice l) => GSFG d l = GSFG (RBBC d l)

instance (Disorder d, Lattice l) => Graph (GSFG d l) where  
  vertices (GSFG r) = [0 .. (fromIntegral (size r) + 1)]
  neighbors (GSFG r) v = fromJust $ IM.lookup v $ networkNeighbors r
  edges (GSFG r) = networkEdges r

instance (Disorder d, Lattice l) => Lattice (GSFG d l) where  
  size (GSFG r) = size r + 2
  numEdges (GSFG r) = 4 * (size r)
  forwardEdges = adjacentEdges
  edgeIx (GSFG r) = mapEdgeIndx r

memoWeights :: (Disorder d, Lattice l) => (RBBC d l -> V.Vector Double) -> (RBBC d l -> IM.IntMap Double)
memoWeights w = let zw = (zip [1..]) . V.toList . w
                 in IM.fromList . zw

weights :: (Disorder d, Lattice l) => RBBC d l -> V.Vector Double
weights r = let js = interactions r
                wi v = let j e = js V.! (fromJust (edgeIx r e) - 1)
                       in (crystalField r) - (sum $ map j (forwardEdges r v))
              in V.fromList $ map wi (vertices r)

networkNeighbors :: (Disorder d, Lattice l) => RBBC d l -> IM.IntMap [Vertex]
networkNeighbors r = let fgn v
                            | v == s = foldr (\(v, wi) ac -> case wi < 0.0 of 
                                                               True -> v: ac
                                                               False -> ac
                                                               ) [] $ zip [1..] $ V.toList wis
                            | v == t = []
                            | otherwise = let innerNNs = map (\e -> snd $ toTuple e) (forwardEdges r v)
                                           in case wis V.! (v - 1) >= 0 of
                                                True -> t:innerNNs
                                                False -> innerNNs
                          in IM.fromList $ zip vs (map fgn vs)
    where wis = weights r
          n = fromIntegral $ size r
          s = 0
          t = fromIntegral (size r) + 1
          vs = [s..t]

        
networkEdges :: (Disorder d, Lattice l) => RBBC d l -> [Edge]
networkEdges r = 
  let wis =  weights r
      s = 0
      t = fromIntegral (size r) + 1 :: Int
      vs = vertices r
      es = edges r
      (sourceEdges,targetEdges) = 
        foldr (\(v,w) ac -> case w < 0 of
                              True -> (fromTuple (s,fromIntegral v):(fst ac), snd ac)
                              False -> (fst ac, fromTuple (fromIntegral v,t):(snd ac))
              ) ([],[]) $ zip vs (V.toList wis) :: ([Edge],[Edge])
   in (sourceEdges ++ es) ++ targetEdges -- ^ Source edges ++ Realization edges ++ Target edges

-- | networkEdges alias
--edges (GSFG r) = networkEdges r

instance (Disorder d, Lattice l) => Network (GSFG d l) where
  capacities (GSFG r) = 
    let wis = weights r
        s = 0 :: Int
        t = fromIntegral (size r) + 1 :: Int
        (sourceCs, targetCs) = 
          V.foldr (\w ac -> case w < 0 of
                            True -> ((-w):(fst ac), 0:(snd ac))
                            False -> (0:(fst ac), w:(snd ac))
                ) ([],[]) wis
             in sourceCs ++ V.toList (interactions r) ++ targetCs
  flows fg = []
  source fg = 0
  sink (GSFG r) = fromIntegral (size r) + 1
  adjacencyMap (GSFG r) = networkNeighbors r

maxFlow :: (Disorder d, Lattice l) => (GSFG d l) -> Double
maxFlow fg = 3.2
        
groundState :: (Disorder d, Lattice l) => RBBC d l -> BCConfiguration
groundState (RBBC d l f) = BCConfiguration (V.fromList []) 
