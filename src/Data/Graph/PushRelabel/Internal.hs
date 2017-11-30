{-|
Module      : PushRelabel - Internal
Description : Maximum Flow - Min Cut - Push relabel algorithm - Definitions
Copyright   : Thodoris Papakonstantinou, 2017
License     : GPL-3
Maintainer  : mail@tpapak.com
Stability   : experimental
Portability : POSIX


 -}

{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Graph.PushRelabel.Internal
  ( 
    Network (..)
  , ResidualGraph (..)
  , NeighborsMap
  , ResidualVertex (..)
  , ResidualEdge (..)
  , Capacity (..)
  , Capacities (..)
  , Flow 
  , Height
  , Excess
  , Level
  , initializeResidualGraph
  , level
  , excess
  , height
  , netFlow
  , push
  , pull
  , getOverflowing
  , inflow
  , outflow
  , updateHeight
  , updateExcess
  , updateEdge
  , sourceEdgesCapacity
  , residualDistances
  , stCut
  ) where

import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as Set
import qualified Control.Monad.Parallel as Par
import Control.Monad

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS


import Data.Graph.Network
import Data.Graph
import qualified Data.Graph.BFS as BFS

type Height = Int
type Excess = Capacity
type Level = Int

data ResidualVertex = ResidualVertex Vertex Level Height Excess
  deriving (Eq)
instance Show ResidualVertex where
  show (ResidualVertex v l h x) =
    "RVertex " ++ show v ++  " level: " ++
      show l ++ " height: " ++
      show h ++ " excess: " ++
      show (fromRational x :: Double)
type ResidualVertices = IM.IntMap ResidualVertex

data ResidualEdge = ResidualEdge Edge Capacity Flow
  deriving (Eq)
instance Show ResidualEdge where
  show (ResidualEdge e c f) =
    "REdge " ++ show e 
      ++  " " ++
      show (fromRational c :: Double)
      ++  " " ++
      show (fromRational f :: Double)
type ResidualEdges = IM.IntMap ResidualEdge

type NeighborsMap = IM.IntMap ([Vertex], [Vertex])

type Overflowing = IM.IntMap Set.IntSet

data ResidualGraph = ResidualGraph { network :: Network
                                   , netVertices :: ResidualVertices
                                   , netEdges :: ResidualEdges 
                                   , netNeighborsMap :: NeighborsMap 
                                   , overflowing :: Overflowing
                                   , steps :: Int
                                   }
                       deriving (Show,Eq)

initializeResidualGraph :: Network -> ResidualGraph
initializeResidualGraph net = 
  let vs = initializeVertices net
      es = initializeEdges net
      neimap = getNetNeighborsMap $ graph net 
   in ResidualGraph { network = net
                    , netVertices = vs 
                    , netEdges = es 
                    , netNeighborsMap = neimap
                    , overflowing = 
                      let ovfs = getOverflowing vs
                          bfs = BFS.bfs (graph net) (source net)
                          maxLevel = BFS.maxLevel bfs
                          fl v =  let (ResidualVertex _ l _ _) = fromJust $ IM.lookup v vs
                                   in l
                       in Set.foldl' 
                            (\ac v -> 
                               IM.adjust (\ps -> Set.insert v ps) (fl v) ac
                            ) (IM.fromList (zip [1..maxLevel] (repeat Set.empty))) ovfs
                    , steps = 0
                    } 

getNetNeighborsMap :: Graph -> NeighborsMap
getNetNeighborsMap g =
  let revneis = getReverseNeighbors (vertices g) (edges g)
      neis v = (neighbors g v, fromJust (IM.lookup v revneis))
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

sourceEdgesCapacity :: Network -> Capacity
sourceEdgesCapacity net = 
  let ses = sourceEdges net
   in sum $ map snd ses

initializeVertices :: Network -> ResidualVertices
initializeVertices net =
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      sh = fromIntegral $ numVertices g
      ses = sourceEdges net
      vs = vertices $ graph net
      flevels = BFS.level $ BFS.bfs (graph net) (source net)
      fl v = fromJust $ IM.lookup v flevels
      zvs = IM.fromList $ 
        zip (vertices g) (map (\v -> 
          ResidualVertex v (fl v) 0 0) $ vertices g)
      (sx, nvs) = foldl' (\(cx,ac) (e,c) -> 
        let v = to e
         in (cx-c, IM.adjust (const (ResidualVertex v (fl v) 0 c)) v ac)) (0, zvs) ses
   in IM.insert s (ResidualVertex s 0 sh sx) nvs

initializeEdges :: Network -> ResidualEdges
initializeEdges net =
  let g = graph net
      cs = capacities net
      s = source net
      t = sink net
      inites = IM.fromList $ map (\(e,c) -> (fromJust $ edgeIndex g e, ResidualEdge e c 0)) (M.toList cs)
      ses = sourceEdges net
   in  foldl' (\ac (e,c) -> IM.insert (fromJust $ edgeIndex g e) (ResidualEdge e c c) ac) inites ses 

getOverflowing :: IM.IntMap ResidualVertex -> Set.IntSet
getOverflowing nvs = 
  let xv (ResidualVertex v _ _ x) = x
      vv (ResidualVertex v _ _ x) = v
   in Set.fromList $ map snd $ IM.toList (IM.map (\nv -> vv nv) (IM.filter (\nv -> xv nv > 0) nvs))

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
                      , (\nt -> updateExcess nt u (xu - xf))
                      , (\nt -> updateExcess nt v (xv + xf))
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
                     , (\nt -> updateExcess nt u (xu + xf))
                     , (\nt -> updateExcess nt v (xv - xf))
                     ]
            in Just g'
         else Nothing 

updateHeight :: ResidualGraph -> Vertex -> Height -> ResidualGraph
updateHeight g v nh =
  let netvs = netVertices g
      nv = fromJust $ IM.lookup v netvs
      x = excess g v
      l = level g v
      s = source $ network g
      t = sink $ network g
  in if v == t || v == s then g
                         else g { netVertices = IM.adjust (const (ResidualVertex v l nh x)) v netvs }

updateExcess :: ResidualGraph -> Vertex -> Excess -> ResidualGraph
updateExcess g v nx =
  let netvs = netVertices g
      nv = fromJust $ IM.lookup v netvs
      h = height g v
      l = level g v
      ovfs = overflowing g
      s = source $ network g
      t = sink $ network g
      newovfs = 
        if v == s || v == t
           then ovfs
           else
             let ovfs' = IM.update (\lvs -> 
                         let lset = Set.delete v lvs
                          in if Set.null lset
                                      then Nothing 
                                      else Just lset) l ovfs
              in if nx == 0
                then 
                  ovfs'
                else 
                  let mlset = IM.lookup l ovfs'
                   in case mlset of 
                        Nothing -> IM.insert l (Set.singleton v) ovfs'
                        Just lset -> IM.adjust (Set.insert v) l ovfs'
   in if v == t then g
                else g { netVertices = IM.insert v (ResidualVertex v l h nx) netvs
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

height :: ResidualGraph -> Vertex -> Height
height rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv l h x) = fromJust $ IM.lookup v (netVertices rg)
   in h

excess :: ResidualGraph -> Vertex -> Excess
excess rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv l h x) = fromJust $ IM.lookup v (netVertices rg)
   in x

level :: ResidualGraph -> Vertex -> Level
level rg v =
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      nvs = fromIntegral $ numVertices g
      (ResidualVertex nv l h x) = fromJust $ IM.lookup v (netVertices rg)
   in l

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
      reds = map (\n -> fromTuple (v,n)) $ fst ns
   in foldl' (\ac e -> (ac + edgeFlow g e)) 0 reds 

  -- | (distance from sink , distance from source (only those that don't connect
  -- to sink))
residualDistances :: ResidualGraph -> (IM.IntMap Int, IM.IntMap Int)
residualDistances rg = 
  let es = map snd (IM.toList $ netEdges rg)
      tres = filter (\(ResidualEdge e c f) -> f < c) es
      tbes = filter (\(ResidualEdge e c f) -> f > 0) es
      tfsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup v ac 
         in case mns of 
               Nothing -> IM.insert v [u] ac
               Just ns -> IM.insert v (u:ns) ac
             ) IM.empty tres
      tsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup u ac 
         in case mns of 
               Nothing -> IM.insert u [v] ac
               Just ns -> IM.insert u (v:ns) ac
             ) tfsatnbs tbes
      sfsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup v ac 
         in case mns of 
               Nothing -> IM.insert u [v] ac
               Just ns -> IM.insert u (v:ns) ac
             ) IM.empty tres
      ssatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup v ac 
         in case mns of 
               Nothing -> IM.insert v [u] ac
               Just ns -> IM.insert v (u:ns) ac
             ) sfsatnbs tbes
      tlvs = BFS.level $ BFS.adjBFS tsatnbs t
      slvs = BFS.level $ BFS.adjBFS ssatnbs s
    in (slvs,tlvs)
  where
    g = graph $ network rg
    s = source $ network rg
    t = sink $ network rg

-- | gives st partition 
stCut :: ResidualGraph -> ([Vertex],[Vertex])
stCut rg = 
  let ts = Set.delete s $ Set.delete t $ Set.fromList $ map fst (IM.toList (snd $ residualDistances rg))
      g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      vs = Set.delete s $ Set.delete t $ Set.fromList $ vertices g
      ss = Set.difference vs ts
   in (Set.toList ss, Set.toList ts)
