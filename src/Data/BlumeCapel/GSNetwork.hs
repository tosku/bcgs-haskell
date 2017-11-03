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
  , gsBCCapacities
  , network'RBBC
  , GSNetwork (..)
  ) where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM

import Data.Graph
import Data.Graph.MaxFlow
import Data.BlumeCapel

-- | Ground state flow graph from rbbc realization
type GSNetwork = Network 

network'RBBC :: RBBC -> GSNetwork 
network'RBBC r = Network { graph = let g = lattice r 
                                    in Graph { vertices = [0 .. (fromIntegral (size r) + 1)]
                                             , edges = networkEdges r
                                             , neighbors = (\v -> fromJust $ IM.lookup v $ networkNeighbors r)
                                             , outEdges = outEdges g
                                             , edgeIndex = mapEdgeIndx g
                                             }
                         , source = 0
                         , sink = size r + 1
                         , capacities = gsBCCapacities r
                         , flow = M.empty
                         }

weights :: RBBC -> IM.IntMap Capacity
weights r = let js = interactions r 
                g = lattice r
                wi v = let j e = fromJust $ M.lookup e js
                        in (fieldCoupling r Up) - (sum $ map j (outEdges g v))
              in IM.fromList $ zip (vertices g) (map wi (vertices g))

networkNeighbors :: RBBC -> IM.IntMap [Vertex]
networkNeighbors r = IM.fromList $ zip vs (map getnn vs)
    where wis = weights r
          n = fromIntegral $ size r
          numSourceEdges = IM.size $ IM.filter (\w -> w < 0) wis
          numTargetEdges = length (networkEdges r) - numInnerEdges - numSourceEdges
          numInnerEdges = numEdges $ lattice r
          s = source (network'RBBC r)
          t = sink (network'RBBC r)
          vs = vertices $ graph (network'RBBC r)
          getnn v 
            | v == s = map to $ take numSourceEdges (networkEdges r)
            | v == t = []
            | otherwise = let innerNNs = map (\e -> snd $ toTuple e) (outEdges (lattice r) v)
                             in case fromJust (IM.lookup v wis) >= 0 of
                                  True -> t:innerNNs
                                  False -> innerNNs

netEdgeCaps :: RBBC -> [(Edge, Capacity)]
netEdgeCaps r = 
  let wis = weights r
      s = 0
      t = size r + 1
      vs = vertices $ lattice r
      es = edges $ lattice r
      js = interactions r
      (sourceEdges,targetEdges) = 
        foldr (\v ac -> let w = toRational $ fromJust $ IM.lookup v wis
                         in case w < 0 of
                              True -> ((fromTuple (s,fromIntegral v),abs(w)):(fst ac), snd ac)
                              False -> (fst ac, (fromTuple (fromIntegral v,t),w):(snd ac))
              ) ([],[]) vs :: ([(Edge,Capacity)],[(Edge,Capacity)])
   in (sourceEdges ++ M.toList js) ++ targetEdges -- ^ Source edges ++ Realization edges ++ Target edges

networkEdges :: RBBC -> [Edge]
networkEdges r = map fst (netEdgeCaps r)

gsBCCapacities :: RBBC -> Capacities
gsBCCapacities r = M.fromList (netEdgeCaps r)

maxFlow :: GSNetwork -> ResidualGraph
maxFlow fg = pushRelabel fg 
