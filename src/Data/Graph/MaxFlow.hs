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
  ( ResidualGraph (..)
  , pushRelabel
  , initializeResidualGraph
  , ResidualVertex (..)
  , ResidualEdge (..)
  , Network (..)
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
data ResidualVertex = ResidualVertex Vertex Height Excess
  deriving (Show,Eq,Ord)

data ResidualEdge = ResidualEdge Edge Capacity Flow
  deriving (Show,Eq,Ord)

type ResidualVertices = IM.IntMap ResidualVertex
type NeighborsMap = IM.IntMap ([Vertex],[Vertex])
type ResidualEdges = IM.IntMap ResidualEdge

data Network = Network { graph :: Graph
                       , source :: Vertex
                       , sink :: Vertex
                       , capacities :: Capacities
                       , flow :: Capacities
                       }
                       deriving (Show,Eq)

data ResidualGraph = ResidualGraph { network :: Network
                                   , netVertices :: ResidualVertices
                                   , netNeighborsMap :: IM.IntMap ([Vertex], [Vertex])
                                   , netEdges :: ResidualEdges 
                                   , preflow :: Flow
                                   , overflowing :: Set.IntSet
                                   , steps :: Int
                                   }
                       deriving (Show,Eq)

initializeResidualGraph :: Network ->  ResidualGraph
initializeResidualGraph net = ResidualGraph { network = net
                                            , netVertices = initializeVertices net
                                            , netNeighborsMap = getNetNeighborsMap $ graph net
                                            , netEdges = initializeEdges net
                                            , preflow = sum $ map snd (sourceEdges net)
                                            , overflowing = initializeOverflowing net
                                            , steps = 0
                                            }

getNetNeighborsMap :: Graph -> IM.IntMap ([Vertex],[Vertex])
getNetNeighborsMap g =
  let neis v = (neighbors g v, fromJust (IM.lookup v (getReverseNeighbors (vertices g) (edges g))))
   in foldl (\ac v -> IM.insert v (neis v) ac) IM.empty (vertices g)

netNeighbors :: NeighborsMap -> Vertex -> ([Vertex],[Vertex]) -- ^ graph and reverse (inward and outward) neighbors
netNeighbors nm v = fromJust $ IM.lookup v nm

getReverseNeighbors :: [Vertex] -> [Edge] -> IM.IntMap [Vertex]
getReverseNeighbors vs es = IM.fromList $ zip vs (map (\v -> map from (filter (\re -> to re == v) es)) vs)

sourceEdges :: Network -> [(Edge,Capacity)]
sourceEdges net = 
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      cap v = fromJust $ M.lookup (Edge s v) cs
    in map (\v -> ((Edge s v), cap v )) (neighbors g s) 

initializeVertices :: Network -> ResidualVertices
initializeVertices net =  
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      sh = fromIntegral $ numVertices g
      ses = sourceEdges net
      zvs = IM.fromList $ zip (vertices g) 
              (map (\v -> ResidualVertex v 0 0) $ vertices g)
      (sx, nvs) = foldl' (\(cx,ac) (e,c) -> let v = to e
                                             in (cx-c, IM.insert v (ResidualVertex v 0 c) ac)) (0, zvs) ses
  in  IM.insert s (ResidualVertex s sh sx) nvs

initializeEdges :: Network -> ResidualEdges
initializeEdges net =
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      inites = IM.fromList $ map (\(e,c) -> (fromJust $ edgeIndex g e, ResidualEdge e c 0)) (M.toList cs)
      ses = sourceEdges net
   in foldl' (\ac (e,c) -> IM.insert (fromJust $ edgeIndex g e) (ResidualEdge e c c) ac) inites ses 

initializeOverflowing :: Network -> Set.IntSet
initializeOverflowing net =
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      ses = sourceEdges net
   in Set.fromList $ map (to . fst) ses

memoize :: (Int -> g) -> (Int -> g)
memoize f = (map f [0 ..] !!) 

pushRelabel :: Network -> ResidualGraph
pushRelabel net = 
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      neimap = getNetNeighborsMap g
      initg = initializeResidualGraph net
      pushNeighbors g v = 
        let (fns, rns) = fromJust $ IM.lookup v neimap
            feds = map (\n -> fromTuple (v,n)) fns
            reds = map (\n -> fromTuple (n,v)) rns
            fg = foldl' (\ac e -> push ac e) g feds
         in foldl' (\ac e -> pull ac e) fg reds
      prl g = g `seq` case Set.null $ overflowing g of 
                        True -> g
                        False -> 
                          let v = Set.findMin $ overflowing g  
                              g' = (pushNeighbors g v) {steps = (steps g) + 1}
                              g'' = g' `seq` case netEdges g' == netEdges g of 
                                               True -> relabel g' v
                                               False -> g'
                            in g'' `seq` prl g''
   in initg `seq` prl initg

push :: ResidualGraph -> Edge -> ResidualGraph
push g e = if hu >= hv + 1 then
                   let xf = min xu (c - f)
                       newuv = ResidualEdge e c (f + xf)
                       newEdges = updateEdge g newuv
                       newu = ResidualVertex u hu (xu - xf)
                       newv = if v /= t then
                                ResidualVertex v hv (xv + xf)
                              else
                                ResidualVertex t 0 0 -- ^ essential for not including target into overflowings
                       newVertices = updateVertices g [newu,newv]
                       newOverflowing = updateOverflowing g [newu,newv]
                       newFlow = getFlow g 
                    in g { netVertices = newVertices
                         , netEdges = newEdges
                         , overflowing = newOverflowing
                         , preflow = newFlow
                         }
                else g
    where u = from e
          v = to e
          t = sink $ network g
          hu = height g u
          hv = height g v
          xu = excess g u 
          xv = excess g v
          c = edgeCapacity g e
          f = edgeFlow g e
          nvs = netVertices g
          evs = overflowing g

pull :: ResidualGraph -> Edge -> ResidualGraph
pull g e = if hv >= hu + 1 then
                   let xf = min xv f
                       newuv = ResidualEdge e c (f - xf)
                       newu = ResidualVertex u hu (xu + xf)
                       newv = ResidualVertex v hv (xv - xf)
                       newEdges = updateEdge g newuv
                       newVertices = updateVertices g [newu,newv]
                       newOverflowing = updateOverflowing g [newu,newv]
                       newFlow = getFlow g
                    in g { netVertices = newVertices
                         , netEdges = newEdges
                         , overflowing = newOverflowing
                         , preflow = newFlow
                         }
                else g
  where u = from e
        v = to e
        hu = height g u
        hv = height g v
        xu = excess g u 
        xv = excess g v
        c = edgeCapacity g e
        f = edgeFlow g e
        nvs = netVertices g
        evs = overflowing g

relabel :: ResidualGraph -> Vertex -> ResidualGraph
relabel g v = let hv  = height g v
                  xv = excess g v
                  ns  = netNeighbors (netNeighborsMap g) v 
                  gnes = map (\n -> fromTuple (v,n)) $ fst ns -- ^ neighboring graph edges
                  rnes = map (\n -> fromTuple (n,v)) $ snd ns -- ^ neighboring reverse edges
                  gcfs = filter (\(e,cf) -> cf > 0) (map (\e -> (e, edgeCapacity g e - edgeFlow g e)) gnes)
                  rcfs = filter (\(e,cf) -> cf > 0) (map (\e -> (e, edgeFlow g e)) rnes)
                  neighborHeights = (map (\(e,c) -> height g (to e)) gcfs) ++ (map (\(e,c) -> height g (from e)) rcfs)
                  newh = 1 + minimum neighborHeights
                in case any (\nh -> hv > nh) neighborHeights || neighborHeights == [] of
                      False -> g { netVertices = updateVertices g [ResidualVertex v newh xv] }
                      True -> g

getFlow :: ResidualGraph -> Flow
getFlow g = let s = source $ network g
                nm = netNeighborsMap g
                ses = map (\v -> fromTuple (s,v)) $ fst (netNeighbors nm s)
            in foldl' (\ac e -> ac + edgeFlow g e) 0 ses
                          
updateOverflowing :: ResidualGraph -> [ResidualVertex] -> Set.IntSet
updateOverflowing g nvs = let oldvs = overflowing g
                           in foldl' (\ac (ResidualVertex v h x) -> if x > 0 then Set.insert v ac else Set.delete v ac) oldvs nvs

updateVertices :: ResidualGraph -> [ResidualVertex] -> ResidualVertices
updateVertices g nvs = let vs = netVertices g :: ResidualVertices
                        in foldl' (\ac nv -> IM.insert (vertex nv) nv ac) vs nvs


updateEdge :: ResidualGraph -> ResidualEdge -> ResidualEdges
updateEdge g (ResidualEdge e c f) = let ne = (ResidualEdge e c f)  
                                        l = graph $ network g
                                        eid = fromJust $ edgeIndex l e
                                        es = netEdges g
                                     in IM.insert eid ne es

vertex :: ResidualVertex -> Vertex
vertex (ResidualVertex v _ _) = v

height :: ResidualGraph -> Vertex -> Height
height g v = let (ResidualVertex nv h e) = fromJust $ IM.lookup v (netVertices g)
              in h

excess :: ResidualGraph -> Vertex -> Capacity
excess g v = let (ResidualVertex nv h e) = fromJust $ IM.lookup v (netVertices g)
              in e

edgeCapacity :: ResidualGraph -> Edge -> Capacity
edgeCapacity g e = let (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ edgeIndex (graph $ network g) e) (netEdges g)
                    in c

edgeFlow :: ResidualGraph -> Edge -> Flow
edgeFlow g e = let l = graph $ network g
                   (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ edgeIndex l e) (netEdges g)
                in f
