{-|
Module      : PushRelabel - Pure
Description : Maximum Flow - Min Cut - Push relabel algorithm - Pure
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Graph.PushRelabel.Pure
  ( ResidualGraph (..)
  , pushRelabel
  , initializeResidualGraph
  , ResidualVertex (..)
  , ResidualEdge (..)
  , Network (..)
  , Capacity (..)
  , Capacities (..)
  , Flow 
  , excess
  , height
  {-, inFlow-}
  , netFlow
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as Set
import qualified Control.Monad.Parallel as Par
import Control.Monad

import Data.Graph
import Data.Graph.BFS

type Capacity = Rational 
type Capacities = M.Map Edge Capacity 
type Flow = Capacity
type Height = Natural
type Excess = Capacity

data ResidualVertex = ResidualVertex !Vertex !Height !Excess
  deriving (Eq)


data ResidualEdge = ResidualEdge !Edge !Capacity !Flow
  deriving (Eq)


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
                                   , netEdges :: ResidualEdges 
                                   , netNeighborsMap :: IM.IntMap ([Vertex], [Vertex])
                                   , overflowing :: Set.IntSet
                                   , steps :: Int
                                   }
                       deriving (Eq)

initializeResidualGraph :: Network -> ResidualGraph
initializeResidualGraph net = 
  let vs = initializeVertices net
      es = initializeEdges net
   in ResidualGraph { network = net
                    , netVertices = vs 
                    , netEdges = es 
                    , netNeighborsMap = getNetNeighborsMap $ graph net 
                    , overflowing = Set.fromList $ getOverflowing $ initializeResidualGraph net
                    , steps = 0 
                    } 

reverseNetwork :: Network -> Network
reverseNetwork net = Network { graph = reverseGraph $ graph net
                             , source = sink net
                             , sink = source net
                             , capacities = reverseCapacities $ capacities net
                             , flow = reverseFlows $ flow net
                             }

reverseCapacities :: Capacities -> Capacities
reverseCapacities cs = M.fromList $ map (\(e,c) -> (reverseEdge e,c)) (M.toList cs)

reverseFlows :: Capacities -> Capacities
reverseFlows fs = M.fromList $ map (\(e,f) -> (reverseEdge e,f)) (M.toList fs)

getNetNeighborsMap :: Graph -> IM.IntMap ([Vertex],[Vertex])
getNetNeighborsMap g =
  let neis v = (neighbors g v, fromJust (IM.lookup v (getReverseNeighbors (vertices g) (edges g))))
   in foldl (\ac v -> IM.insert v (neis v) ac) IM.empty (vertices g)

netNeighbors :: NeighborsMap -> Vertex -> ([Vertex],[Vertex]) -- ^ graph and reverse (inward and outward) neighbors
netNeighbors nm v = fromJust $ IM.lookup v nm

sourceEdges :: Network -> [(Edge,Capacity)]
sourceEdges net = 
  let g = graph net
      cs = capacities net
      s = source net
      cap v = fromJust $ M.lookup (Edge s v) cs
    in map (\v -> ((Edge s v), cap v )) (neighbors g s) 

reverseSinkEdges :: Network -> [(Edge,Capacity)]
reverseSinkEdges net = 
  let g = graph net
      cs = capacities net
      t = sink net
      cap v = fromJust $ M.lookup (Edge v t) cs
   in map (\v -> ((Edge t v), cap v )) (neighbors (reverseGraph g) t) 


initializeVertices :: Network -> ResidualVertices
initializeVertices net =
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      sh = fromIntegral $ numVertices g
      ses = sourceEdges net
      zvs = IM.fromList $ zip (vertices g) (map (\v -> ResidualVertex v 1 0) $ vertices g)
      (sx, nvs) = foldl' (\(cx,ac) (e,c) -> let v = to e
                                              in (cx-c, IM.insert v (ResidualVertex v 2 c) ac)) (0, zvs) ses
   in  IM.insert s (ResidualVertex s sh sx) nvs

initializeEdges :: Network -> ResidualEdges
initializeEdges net =
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      inites = IM.fromList $ map (\(e,c) -> (fromJust $ edgeIndex g e, ResidualEdge e c 0)) (M.toList cs)
      ses = sourceEdges net
   in  foldl' (\ac (e,c) -> IM.insert (fromJust $ edgeIndex g e) (ResidualEdge e c c) ac) inites ses 

getOverflowing :: ResidualGraph -> [Vertex]
getOverflowing rg = 
  let vs = vertices $ graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      xvs = map (\v -> (v, excess rg v)) vs
   in  map fst $ filter (\(v,x) -> x /= 0 && v /= s && v /= t) xvs

pushRelabel :: Network -> Flow
pushRelabel net =
  let !initg = initializeResidualGraph net
      res = prl initg
   in netFlow res
  where 
      g = graph net
      prl :: ResidualGraph -> ResidualGraph
      prl rg =
        let ovfs = overflowing rg
            u = Set.findMax ovfs
         in if Set.null ovfs
         {-in if Set.null ovfs || steps rg > 100000-}
               then  rg 
               else  
                  let !rg' = case discharge rg u of
                               Just rg'' -> rg''
                               Nothing -> case relabel rg u of
                                            Nothing -> rg
                                            Just g'' ->  g''
                   in prl (rg' {steps = steps rg + 1})
                  
discharge :: ResidualGraph -> Vertex -> Maybe ResidualGraph
discharge g v =
  let neimap = netNeighborsMap g
      (fns, rns) = fromJust $ IM.lookup v neimap
      feds = map (\n -> fromTuple (v,n)) fns
      reds = map (\n -> fromTuple (n,v)) rns
      (changed, g') = foldl' (\ac e -> (\mv -> 
        case mv of 
          Nothing -> ac
          Just g'' -> (True,g'')) (push g e)) (False,g) feds
      (changed',g'') = foldl' (\ac e -> (\mv -> 
        case mv of 
          Nothing -> ac
          Just g''' -> (True,g''')) (pull g e)) (changed, g') reds
       in if changed' 
             then 
               Just g''
             else
               Nothing

push :: ResidualGraph -> Edge -> Maybe ResidualGraph
push g e =  
  let u = from e
      v = to e
      hu = height g u
      hv = height g v 
      xu = excess g u 
      xv = excess g v
      c = edgeCapacity g e
      f = edgeFlow g e
      nvs = netVertices g
      xf = min xu (c - f)
   in if (hu == hv + 1) && xf > 0
         then
           let g' = foldr (\f ac -> f ac) g
                      [ (\nt -> updateEdge nt e (f + xf))
                      , (\nt -> updateVertex nt u hu (xu - xf))
                      , (\nt -> updateVertex nt v hv (xv + xf))
                      ]
            in Just g'
         else Nothing 

pull :: ResidualGraph -> Edge -> Maybe ResidualGraph
pull g e = 
  let u = from e
      v = to e
      hu = height g u
      hv = height g v 
      xu = excess g u 
      xv = excess g v
      c = edgeCapacity g e
      f = edgeFlow g e
      nvs = netVertices g
      xf = min xv f
   in if (hv == hu + 1)  && xf > 0 
         then
           let g' = foldr (\f ac -> f ac) g
                     [ (\nt -> updateEdge nt e (f - xf))
                     , (\nt -> updateVertex nt u hu (xu + xf))
                     , (\nt -> updateVertex nt v hv (xv - xf))
                     ]
            in Just g'
         else Nothing 

relabel :: ResidualGraph -> Vertex -> Maybe ResidualGraph
relabel g v =
  let ns  = netNeighbors (netNeighborsMap g) v 
      gnes = map (\n -> fromTuple (v,n)) $ fst ns -- ^ neighboring graph edges
      rnes = map (\n -> fromTuple (n,v)) $ snd ns -- ^ neighboring reverse edges
      gacfs = map (\e -> (e, edgeCapacity g e - edgeFlow g e)) gnes
      gcfs = filter (\(e, cf) -> cf > 0) gacfs
      racfs = map (\e -> (e, edgeFlow g e)) rnes
      rcfs = filter (\(e,cf) -> cf > 0) racfs
      hv = height g v
      xv = excess g v
      neighborHeights = (map (\(e,c) -> height g (to e)) gcfs) ++ (map (\(e,c) -> height g (from e)) rcfs)
      newh = 1 + minimum neighborHeights
   in case any (\nh -> hv > nh) neighborHeights || null neighborHeights of
        True -> Nothing
        False -> Just $ updateVertex g v newh xv

{-saturatedReverseNeighbors :: [ResidualEdgeP] -> IM.IntMap [Vertex]-}
{-saturatedReverseNeighbors es = -}
  {-let res = filter (\(ResidualEdgeP e c f) -> f < c) es-}
   {-in foldl' (\ac (ResidualEdgeP e c f) -> let u = from e-}
                                               {-v = to e-}
                                               {-mns = IM.lookup v ac-}
                                            {-in case mns of-}
                                                 {-Nothing -> IM.insert v [u] ac-}
                                                 {-Just ns -> IM.insert v (u:ns) ac-}
             {-) IM.empty res-}

updateVertex :: ResidualGraph -> Vertex -> Height -> Excess -> ResidualGraph
updateVertex g v nh nx =
  let netvs = netVertices g
      nv = fromJust $ IM.lookup v netvs
      ovfs = overflowing g
      s = source $ network g
      t = sink $ network g
      newovfs = 
        if v == s || v ==t
           then ovfs
           else
             if nx == 0
                then Set.delete v ovfs
                else Set.insert v ovfs
   in if v == t then g
                else g { netVertices = IM.insert v (ResidualVertex v nh nx) netvs
                       , overflowing = newovfs
                       } 

updateEdge :: ResidualGraph -> Edge -> Flow -> ResidualGraph
updateEdge g e f =
  let l = graph $ network g
      es = netEdges g
      eid = fromJust $ edgeIndex l e
      (ResidualEdge e' c f') = fromJust $ IM.lookup eid es
   in g { netEdges = IM.adjust (const (ResidualEdge e c f)) eid es
        }

netFlow :: ResidualGraph -> Flow
netFlow g = inflow g (sink (network g))

vertex :: ResidualVertex -> Vertex
vertex (ResidualVertex v _ _) = v

height :: ResidualGraph -> Vertex -> Height
height rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv h x) = fromJust $ IM.lookup v (netVertices rg)
   in h


excess :: ResidualGraph -> Vertex -> Excess
excess rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv h x) = fromJust $ IM.lookup v (netVertices rg)
   in x



edgeCapacity :: ResidualGraph -> Edge -> Capacity
edgeCapacity g e = let (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ edgeIndex (graph $ network g) e) (netEdges g)
                    in c 

edgeFlow :: ResidualGraph -> Edge -> Flow
edgeFlow g e = let (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ edgeIndex (graph $ network g) e) (netEdges g)
                in f 

inflow :: ResidualGraph -> Vertex -> Flow
inflow g v =
  let ns  = netNeighbors (netNeighborsMap g) v 
      reds = map (\n -> fromTuple (n,v)) $ snd ns
   in foldl' (\ac e -> (ac + edgeFlow g e)) 0 reds 

outflow :: ResidualGraph -> Vertex -> Flow
outflow g v =
  let ns  = netNeighbors (netNeighborsMap g) v 
      reds = map (\n -> fromTuple (n,v)) $ fst ns
   in foldl' (\ac e -> (ac + edgeFlow g e)) 0 reds 
