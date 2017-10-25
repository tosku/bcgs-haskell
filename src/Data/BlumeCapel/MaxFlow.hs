{-|
Module      : MaxFlow
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


module Data.BlumeCapel.MaxFlow
  ( weights
  , maxFlow
  , groundState  
  , GSFG (..)
  , FlowGraph (..)
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF

import Data.Grid
import Data.BlumeCapel


class Lattice f => FlowGraph f where
  source :: f -> Vertex
  sink :: f -> Vertex
  -- | should respect the `edges` function's ordering: 
  -- Source ++ Realization ++ Target
  capacities :: f -> [Double]
  flows :: f -> [Double] -- ^ should also respect the `edges` function's ordering

-- | Ground state flow graph from rbbc realization
data (Disorder d, Lattice l) => GSFG d l = GSFG (RBBC d l)

instance (Disorder d, Lattice l) => Graph (GSFG d l) where  
  vertices (GSFG r) = [0 .. (fromIntegral (size r) + 1)]
  edges (GSFG r) = flowGraphEdges r
  neighbors (GSFG r) v 
    | v == 0 = vertices r
    | v == (fromIntegral (size r) + 1) = []
    | otherwise = map (\e -> snd $ toTuple e) (forwardEdges r v)
  adjacentEdges (GSFG r) v
    | v == s = map (\v -> fromTuple (s, v)) vs
    | v == t = []
    | otherwise = forwardEdges r v ++ [fromTuple (v,t)]
    where 
      s = 0
      t = fromIntegral (size r) + 1 :: Int
      vs = vertices r

instance (Disorder d, Lattice l) => Lattice (GSFG d l) where  
  size (GSFG r) = size r + 2
  numEdges (GSFG r) = 4 * (size r)
  forwardEdges = adjacentEdges
  edgeIx (GSFG r) = mapEdgeIndx r

weights :: (Disorder d, Lattice l) => RBBC d l -> V.Vector Double
weights r = let js = interactions r
                wi v = let j e = js V.! (fromJust (edgeIx r e) - 1)
                       in (crystalField r) - (sum $ map j (forwardEdges r v))
              in V.fromList $ map wi (vertices r)
        
flowGraphEdges :: (Disorder d, Lattice l) => RBBC d l -> [Edge]
flowGraphEdges r = 
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

instance (Disorder d, Lattice l) => FlowGraph (GSFG d l) where
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


maxFlow :: (Disorder d, Lattice l) => (GSFG d l) -> String
maxFlow fg =
  let vs = map (\v -> (v,())) $ vertices fg :: [G.UNode]
      es = map (\((f,t),c) -> (f,t,c)) $ zip (map toTuple (edges fg)) (capacities fg) :: [G.LEdge Double]
      mfg = G.mkGraph vs es :: I.Gr () Double
   --in  G.prettify mfg
   in show $ MF.maxFlow mfg (source fg) (sink fg)
        
groundState :: (Disorder d, Lattice l) => RBBC d l -> BCConfiguration
groundState (RBBC d l f) = BCConfiguration (V.fromList []) 
