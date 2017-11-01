{-|
Module      : PushRelabel
Description : Maximum Flow - Min Cut - Push relabel algorithm
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}

{-# LANGUAGE Rank2Types #-}

module Data.Graph.MaxFlow
  ( Network (..)
  , pushRelabel
  , initializeNetwork
  , NetworkVertex (..)
  , NetworkEdge (..)
  , Capacity (..)
  , Capacities (..)
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as Set

import Data.Graph

type Capacity = Rational 
type Capacities = M.Map Edge Capacity 
type Flow = Capacity
type Height = Natural
type Excess = Capacity
data NetworkVertex = NetworkVertex Vertex Height Excess
  deriving (Show,Eq,Ord)

data NetworkEdge = NetworkEdge Edge Capacity Flow
  deriving (Show,Eq,Ord)

type ResidualEdge = NetworkEdge 
type NetworkVertices = IM.IntMap NetworkVertex
type NeighborsMap = IM.IntMap ([Vertex],[Vertex])
type NetworkEdges = IM.IntMap NetworkEdge

data Network = Network { source :: Vertex
                       , sink :: Vertex
                       , netVertices :: NetworkVertices
                       , netEdges :: NetworkEdges 
                       , flow :: Flow
                       , overflowing :: Set.IntSet
                       , steps :: Int
                       }
                       deriving (Show,Eq,Ord)

initializeNetwork :: Graph g => g -> Capacities -> Vertex -> Vertex -> Network
initializeNetwork g cs s t = 
  Network { source = s
          , sink = t
          , netVertices = initializeVertices g cs s t
          , netEdges = initializeEdges g cs s t
          , flow = sum $ map snd (sourceEdges g cs s t)
          , overflowing = initializeOverflowing g cs s t
          , steps = 0
          }

getNetNeighborsMap :: Graph g => g -> IM.IntMap ([Vertex],[Vertex])
getNetNeighborsMap g =
  let neis v = (neighbors g v, fromJust (IM.lookup v (getReverseNeighbors (vertices g) (edges g))))
   in foldl (\ac v -> IM.insert v (neis v) ac) IM.empty (vertices g)

netNeighbors :: NeighborsMap -> Vertex -> ([Vertex],[Vertex]) -- ^ graph and reverse (inward and outward) neighbors
netNeighbors nm v = fromJust $ IM.lookup v nm

getReverseNeighbors :: [Vertex] -> [Edge] -> IM.IntMap [Vertex]
getReverseNeighbors vs es = IM.fromList $ zip vs (map (\v -> map from (filter (\re -> to re == v) es)) vs)

sourceEdges :: Graph g => g -> Capacities -> Vertex -> Vertex -> [(Edge,Capacity)]
sourceEdges g cs s t = let cap v = fromJust $ M.lookup (Edge s v) cs
                        in map (\v -> ((Edge s v), cap v )) (neighbors g s) 

initializeVertices :: Graph g => g -> Capacities -> Vertex -> Vertex -> NetworkVertices
initializeVertices g cs s t = 
  let sh = fromIntegral $ numVertices g
      ses = sourceEdges g cs s t
      zvs = IM.fromList $ zip (vertices g) 
              (map (\v -> NetworkVertex v 0 0) $ vertices g)
      (sx, nvs) = foldl' (\(cx,ac) (e,c) -> let v = to e
                                             in (cx-c, IM.insert v (NetworkVertex v 0 c) ac)) (0, zvs) ses
  in  IM.insert s (NetworkVertex s sh sx) nvs

initializeEdges :: Graph g => g -> Capacities -> Vertex -> Vertex -> NetworkEdges
initializeEdges g cs s t =
  let inites = IM.fromList $ map (\(e,c) -> (fromJust $ edgeIndex g e, NetworkEdge e c 0)) (M.toList cs)
      ses = sourceEdges g cs s t
   in foldl' (\ac (e,c) -> IM.insert (fromJust $ edgeIndex g e) (NetworkEdge e c c) ac) inites ses 

initializeOverflowing :: Graph g => g -> Capacities -> Vertex -> Vertex -> Set.IntSet
initializeOverflowing g cs s t =
  let ses = sourceEdges g cs s t
   in Set.fromList $ map (to . fst) ses

memoize :: (Int -> g) -> (Int -> g)
memoize f = (map f [0 ..] !!) 

pushRelabel :: Graph l => l -> Capacities -> Vertex -> Vertex -> Network
pushRelabel lat cs s t = 
  let neimap = getNetNeighborsMap lat
      initg = initializeNetwork lat cs s t
      pushNeighbors g v = 
        let (fns, rns) = fromJust $ IM.lookup v neimap
            feds = map (\n -> fromTuple (v,n)) fns
            reds = map (\n -> fromTuple (n,v)) rns
            fg = foldl' (\ac e -> push lat ac neimap e) g feds
         in foldl' (\ac e -> pull lat ac neimap e) fg reds
      prl g = g `seq` case Set.null $ overflowing g of 
                        True -> g
                        False -> 
                          let v = Set.findMin $ overflowing g  
                              g' = (pushNeighbors g v) {steps = (steps g) + 1}
                              g'' = g' `seq` case netEdges g' == netEdges g of 
                                               True -> relabel lat g' neimap v
                                               False -> g'
                            in g'' `seq` prl g''
   in initg `seq` prl initg

push :: Graph g => g -> Network -> NeighborsMap -> Edge -> Network
push l g nm e = if hu >= hv + 1 then
                   let xf = min xu (c - f)
                       newuv = NetworkEdge e c (f + xf)
                       newEdges = updateEdge l g newuv
                       newu = NetworkVertex u hu (xu - xf)
                       newv = if v /= t then
                                NetworkVertex v hv (xv + xf)
                              else
                                NetworkVertex t 0 0 -- ^ essential for not including target into overflowings
                       newVertices = updateVertices g [newu,newv]
                       newOverflowing = updateOverflowing g [newu,newv]
                       newFlow = getFlow l g nm
                    in g { netVertices = newVertices
                         , netEdges = newEdges
                         , overflowing = newOverflowing
                         , flow = newFlow
                         }
                else g
    where u = from e
          v = to e
          t = sink g
          hu = height g u
          hv = height g v
          xu = excess g u 
          xv = excess g v
          c = edgeCapacity l g e
          f = edgeFlow l g e
          nvs = netVertices g
          evs = overflowing g

pull :: Graph g => g -> Network -> NeighborsMap -> Edge -> Network
pull l g nm e = if hv >= hu + 1 then
                   let xf = min xv f
                       newuv = NetworkEdge e c (f - xf)
                       newu = NetworkVertex u hu (xu + xf)
                       newv = NetworkVertex v hv (xv - xf)
                       newEdges = updateEdge l g newuv
                       newVertices = updateVertices g [newu,newv]
                       newOverflowing = updateOverflowing g [newu,newv]
                       newFlow = getFlow l g nm
                    in g { netVertices = newVertices
                         , netEdges = newEdges
                         , overflowing = newOverflowing
                         , flow = newFlow
                         }
                else g
  where u = from e
        v = to e
        hu = height g u
        hv = height g v
        xu = excess g u 
        xv = excess g v
        c = edgeCapacity l g e
        f = edgeFlow l g e
        nvs = netVertices g
        evs = overflowing g

relabel :: Graph g => g -> Network -> NeighborsMap -> Vertex -> Network
relabel l g nm v = let hv  = height g v
                       xv = excess g v
                       ns  = netNeighbors nm v 
                       gnes = map (\n -> fromTuple (v,n)) $ fst ns -- ^ neighboring graph edges
                       rnes = map (\n -> fromTuple (n,v)) $ snd ns -- ^ neighboring reverse edges
                       gcfs = filter (\(e,cf) -> cf > 0) (map (\e -> (e, edgeCapacity l g e - edgeFlow l g e)) gnes)
                       rcfs = filter (\(e,cf) -> cf > 0) (map (\e -> (e, edgeFlow l g e)) rnes)
                       neighborHeights = (map (\(e,c) -> height g (to e)) gcfs) ++ (map (\(e,c) -> height g (from e)) rcfs)
                       newh = 1 + minimum neighborHeights
                    in case any (\nh -> hv > nh) neighborHeights || neighborHeights == [] of
                          False -> g { netVertices = updateVertices g [NetworkVertex v newh xv] }
                          True -> g

getFlow :: Graph g => g -> Network -> NeighborsMap -> Flow
getFlow l g nm = let s = source g
                     ses = map (\v -> fromTuple (s,v)) $ fst (netNeighbors nm s)
                  in foldl' (\ac e -> ac + edgeFlow l g e) 0 ses
                          
updateOverflowing :: Network -> [NetworkVertex] -> Set.IntSet
updateOverflowing g nvs = let oldvs = overflowing g
                           in foldl' (\ac (NetworkVertex v h x) -> if x > 0 then Set.insert v ac else Set.delete v ac) oldvs nvs

updateVertices :: Network -> [NetworkVertex] -> NetworkVertices
updateVertices g nvs = let vs = netVertices g :: NetworkVertices
                        in foldl' (\ac nv -> IM.insert (vertex nv) nv ac) vs nvs


updateEdge :: Graph g => g -> Network -> NetworkEdge -> NetworkEdges
updateEdge l g (NetworkEdge e c f) = let ne = (NetworkEdge e c f)  
                                         eid = fromJust $ edgeIndex l e
                                         es = netEdges g
                                      in IM.insert eid ne es

vertex :: NetworkVertex -> Vertex
vertex (NetworkVertex v _ _) = v

height :: Network -> Vertex -> Height
height g v = let (NetworkVertex nv h e) = fromJust $ IM.lookup v (netVertices g)
              in h

excess :: Network -> Vertex -> Capacity
excess g v = let (NetworkVertex nv h e) = fromJust $ IM.lookup v (netVertices g)
              in e

edgeCapacity :: Graph g => g -> Network -> Edge -> Capacity
edgeCapacity l g e = let (NetworkEdge ne c f) = fromJust $ IM.lookup (fromJust $ edgeIndex l e) (netEdges g)
                      in c

edgeFlow :: Graph g => g -> Network -> Edge -> Flow
edgeFlow l g e = let (NetworkEdge ne c f) = fromJust $ IM.lookup (fromJust $ edgeIndex l e) (netEdges g)
                  in f
