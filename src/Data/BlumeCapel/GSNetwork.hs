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
  , gsBCCapacities
  , GSFG (..)
  ) where

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Data.Vector as V
import qualified Data.IntMap.Strict as IM
import qualified Data.Graph.MaxFlow as PR

import Data.Graph
import Data.BlumeCapel

-- | Ground state flow graph from rbbc realization
data (Disorder d, Graph l) => GSFG d l = GSFG (RBBC d l)
  deriving (Eq)

instance (Disorder d, Graph l) => Graph (GSFG d l) where  
  vertices (GSFG r) = [0 .. (fromIntegral (size r) + 1)]
  edges (GSFG r) = networkEdges r
  neighbors r v = fromJust $ IM.lookup v $ networkNeighbors r
  outEdges (GSFG r) = outEdges r
  edgeIndex = mapEdgeIndx

weights :: (Disorder d, Graph l) => RBBC d l -> IM.IntMap Double
weights r = let js = interactions r 
                wi v = let j e = fromJust $ M.lookup e js
                       in (crystalField r) - (sum $ map j (outEdges r v))
              in IM.fromList $ zip (vertices r) (map wi (vertices r))

networkNeighbors :: (Disorder d, Graph l) => GSFG d l -> IM.IntMap [Vertex]
networkNeighbors (GSFG r) = IM.fromList $ zip vs (map getnn vs)
    where wis = weights r
          n = fromIntegral $ size r
          numSourceEdges = IM.size $ IM.filter (\w -> w < 0) wis
          numTargetEdges = numEdges (GSFG r) - numInnerEdges - numSourceEdges
          numInnerEdges = numEdges r
          s = source (GSFG r)
          t = sink (GSFG r)
          vs = vertices (GSFG r)
          getnn v 
            | v == s = map to $ take numSourceEdges (networkEdges r)
            | v == t = []
            | otherwise = let innerNNs = map (\e -> snd $ toTuple e) (outEdges r v)
                             in case fromJust (IM.lookup v wis) >= 0 of
                                  True -> t:innerNNs
                                  False -> innerNNs

        
netEdgeCaps :: (Disorder d, Graph l) => RBBC d l -> [(Edge,PR.Capacity)]
netEdgeCaps r = 
  let wis =  weights r
      s = source (GSFG r)
      t = sink (GSFG r)
      vs = vertices r
      es = edges r
      js = M.map toRational $ interactions r
      (sourceEdges,targetEdges) = 
        foldr (\v ac -> let w = toRational $ fromJust $ IM.lookup v wis
                         in case w < 0 of
                              True -> ((fromTuple (s,fromIntegral v),abs(w)):(fst ac), snd ac)
                              False -> (fst ac, (fromTuple (fromIntegral v,t),w):(snd ac))
              ) ([],[]) vs :: ([(Edge,PR.Capacity)],[(Edge,PR.Capacity)])
   in (sourceEdges ++ M.toList js) ++ targetEdges -- ^ Source edges ++ Realization edges ++ Target edges

source (GSFG r) = 0
sink (GSFG r) = fromIntegral (size r) + 1 :: Int

networkEdges :: (Disorder d, Graph l) => (RBBC d l) -> [Edge]
networkEdges r = map fst (netEdgeCaps r)

gsBCCapacities :: (Disorder d, Graph l) => (GSFG d l) -> PR.Capacities
gsBCCapacities (GSFG r) = M.fromList (netEdgeCaps r)

maxFlow :: (Disorder d, Graph l) => (GSFG d l) -> PR.Network
maxFlow fg = PR.pushRelabel fg (gsBCCapacities fg) (source fg) (sink fg)
        
groundState :: (Disorder d, Graph l) => RBBC d l -> BCConfiguration
groundState (RBBC d l f) = BCConfiguration (V.fromList []) 
