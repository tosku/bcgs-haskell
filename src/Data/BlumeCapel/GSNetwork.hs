{-|
Module      : GSNetwork
Description : The reduction of finding the ground state Blume-Capel realization to the max-flow of a flow graph 
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX
|-}

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE BangPatterns #-}


module Data.BlumeCapel.GSNetwork
  ( weights
  , gsBCCapacities
  , network'RBBC
  , groundState
  , getGSMag
  , getGSEnergy
  , Network (..)
  , GroundState (..)
  ) where

import qualified GHC.Generics as Gen

import Data.List
import Data.Maybe
import Data.Either.Unwrap
import qualified Data.Map.Lazy as M
import qualified Data.Vector as V
import qualified Data.IntMap.Lazy as IM

import Data.BlumeCapel

import Data.Graph.AdjacencyList
import Data.Graph.AdjacencyList.Network
import Data.Graph.AdjacencyList.PushRelabel.Pure

network'RBBC :: RBBC -> Network 
network'RBBC r = 
  Network { graph = 
              let vs = [0 .. (fromIntegral (size r) + 1)]
                  neis = (\v -> fromJust $ IM.lookup v $ networkNeighbors r)
               in createGraph vs neis
          , source = 0
          , sink = size r + 1
          , capacities = gsBCCapacities r
          , flow = M.empty
          }

weights :: RBBC -> IM.IntMap Capacity
weights r = let js = interactions r 
                g = lattice r
                wi v = let j e = fromJust $ M.lookup e js
                        in (fieldCoupling r Up) - (sum $ map (\n -> j (fromTuple (v,n))) (neighbors g v))
              in IM.fromList $ zip (vertices g) (map wi (vertices g))

netEdgeIx r (Edge f t) = IM.lookup t $ fromJust (IM.lookup f (netEdgeMap r))

netEdgeMap :: RBBC -> IM.IntMap (IM.IntMap Int)
netEdgeMap r =
  let els = zip (networkEdges r) [1..]
   in foldl' 
        (\ac ((Edge f t), i) 
          -> let med = IM.lookup f ac 
              in case med of 
                   Just ed 
                     -> IM.insert f (IM.insert t i ed) ac  
                   Nothing 
                     -> IM.insert f (IM.singleton t i) ac 
        ) IM.empty els 

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
            | v == s = map to (fst $ sourceTargetEdges r)
            | v == t = []
            | otherwise = let !innerNNs = map (\e -> snd $ toTuple e) (map (\n -> fromTuple (v,n)) (neighbors (lattice r) v))
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
   in (M.toList js ++ sourceEdges) ++ targetEdges -- ^ Source edges ++ Realization edges ++ Target edges

sourceTargetEdges :: RBBC -> ([Edge],[Edge])
sourceTargetEdges r = 
  let wis = weights r
      s = 0
      t = size r + 1
      vs = vertices $ lattice r
      es = edges $ lattice r
      js = interactions r
      (sourceEdges,targetEdges) = 
        foldr (\v ac -> let w = toRational $ fromJust $ IM.lookup v wis
                         in case w < 0 of
                              True -> (fromTuple (s,fromIntegral v):(fst ac), snd ac)
                              False -> (fst ac, (fromTuple (fromIntegral v,t)):(snd ac))
              ) ([],[]) vs :: ([Edge],[Edge])
   in (sourceEdges,targetEdges)

networkEdges :: RBBC -> [Edge]
networkEdges r = map fst (netEdgeCaps r)

gsBCCapacities :: RBBC -> Capacities
gsBCCapacities r = M.fromList (netEdgeCaps r)

gsConfiguration :: ResidualGraph -> BCConfiguration
gsConfiguration rg = 
  let (svs,tvs) = stCut rg
   in SpinConfiguration $ IM.fromList 
   $ map (\v -> (v,Up)) svs ++ map (\v -> (v,Zero)) tvs

getGSMag :: GroundState -> Double
getGSMag gs = 
  let mmag = (getMagnetization . configuration . replica) gs
      n = size $ realization $ replica gs
   in (fromRational mmag) / (fromIntegral n)

getGSEnergy :: GroundState -> Double
getGSEnergy gs = 
  let enn = cutEnergy gs
      n = size $ realization $ replica gs
   in (fromRational enn) / (fromIntegral n)

data GroundState = GroundState { replica :: RBBCReplica
                               , cutEnergy :: Energy
                               }
                               deriving (Eq,Show,Gen.Generic)

groundState :: RBBC -> GroundState
groundState real = 
  let net = network'RBBC real
      scaps = sourceEdgesCapacity net
      rg = fromRight $ pushRelabel net
      cen = netFlow rg - scaps
      conf = gsConfiguration rg
   in GroundState { replica = replica'RBBC real conf
                  , cutEnergy = cen
                  }

