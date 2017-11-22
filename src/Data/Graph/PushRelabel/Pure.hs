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

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS


import Data.Graph
import Data.Graph.BFS

type Capacity = Rational 
type Capacities = M.Map Edge Capacity 
type Flow = Capacity
type Height = Int
type Excess = Capacity

data ResidualVertex = ResidualVertex !Vertex !Height !Excess
  deriving (Eq)

instance Show ResidualVertex where
  show (ResidualVertex v h x) =
    "RVertex " ++ show v ++  " " ++
      show h ++ " " ++
      show (fromRational x :: Double)

data ResidualEdge = ResidualEdge !Edge !Capacity !Flow
  deriving (Eq)

instance Show ResidualEdge where
  show (ResidualEdge e c f) =
    "REdge " ++ show e 
      ++  " " ++
      show (fromRational c :: Double)
      ++  " " ++
      show (fromRational f :: Double)


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
                                   , overflowing :: IM.IntMap Set.IntSet
                                   , steps :: Int
                                   }
                       deriving (Show,Eq)

initializeResidualGraph :: Network -> ResidualGraph
initializeResidualGraph net = 
  let vs = initializeVertices net
      es = initializeEdges net
   in ResidualGraph { network = net
                    , netVertices = vs 
                    , netEdges = es 
                    , netNeighborsMap = getNetNeighborsMap $ graph net 
                    , overflowing = IM.singleton 0 (Set.fromList $ getOverflowing $ initializeResidualGraph net)
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
      zvs = IM.fromList $ zip (vertices g) (map (\v -> if v == t then ResidualVertex t 0 0 else ResidualVertex v 0 0) $ vertices g)
      (sx, nvs) = foldl' (\(cx,ac) (e,c) -> let v = to e
                                             in (cx-c, IM.adjust (const (ResidualVertex v 0 c)) v ac)) (0, zvs) ses
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
   in map fst $ filter (\(v,x) -> x /= 0 && v /= s && v /= t) xvs

pushRelabel :: Network -> IO (Either String ResidualGraph)
pushRelabel net = do
  let !initg = initializeResidualGraph net
  {-let res = initg-}
  {-let res = bfsRelabel initg-}
  {-let res = prl initg-}
  {-let res = prl $ bfsRelabel initg-}
  {-let res = prePush initg-}
  {-let res = bfsRelabel $ prePush initg-}
  {-let res = prl $ bfsRelabel $ prePush initg-}
  {-let res = prePush $ prePull $ bfsRelabel $ prePush initg-}
  {-let res = prePush $ prePull $ prePush $ prePull $ bfsRelabel $ prePush initg-}
  {-let res = prePush $ prePull $ bfsRelabel $ prePush $ prePull $ bfsRelabel $ prePush $ prePull $ bfsRelabel $ prePush initg-}
  let res = argalios initg 0
  let nvs = vertices $ graph $ network res
  let s = source net
  let t = sink net
  let insouts = filter (\v -> v /= s && v /= t && inflow res v < outflow res v) nvs
  let xsflows = filter (\v -> v /= s && v /= t && inflow res v - outflow res v /= excess res v) nvs
  let ofvs = IM.foldl (\ac ovs -> Set.union ac ovs) Set.empty $ overflowing res
  let notofvs = filter (\ ov -> 
                          let (ResidualVertex v h x) = fromJust (IM.lookup ov (netVertices res)) 
                              mh = (IM.lookup h (overflowing res)) 
                           in case mh of
                                Nothing -> True
                                Just os -> not $ Set.member ov os
                       ) $ getOverflowing res
  {-let notofvs = getOverflowing res-}
  let errovfs = Set.filter (\v -> excess res v == 0) ofvs
  if null insouts && null xsflows && Set.null errovfs && null notofvs
      then return $ Right res
      else 
        if not $ null insouts 
              then return $ Left $ "Error Inflow < Outflow " ++ show insouts
              else
                if not $ null xsflows 
                  then return $ Left $ "Error vertex excess " ++ show xsflows
                  else
                    if not $ Set.null errovfs 
                      then return $ Left $ "Error not really overflowing " ++ show errovfs
                      else return $ Left $ "Error not in overflowing " ++ show notofvs
                        ++ " overflowings are " ++ show (overflowing res)
                        ++ " nevertices are " ++ show (netVertices res)
                  
prl :: ResidualGraph -> ResidualGraph
prl !rg =
  let g = graph $ network rg
      sh = numVertices g
      ovfs = overflowing rg
      (h,ovs) = IM.findMax ovfs
      u = Set.findMin ovs
      osteps = steps rg
      rg' = case discharge rg u of
               Just rg'' -> rg''
               {-Nothing -> bfsRelabel $ reRelabel rg-}
               Nothing -> let !rg''' = relabel rg u
                           in case rg''' of
                                Nothing -> rg {steps = (negate u) - 1 }
                                Just g'' ->  g''
   {-in if IM.null ovfs-}
  in if IM.null ovfs || osteps < 0
         then rg 
         else  
           if osteps `mod` (sh+2) == 0
              {-then prl (bfsRelabel $ rg' {steps = steps rg' + 1})-}
              then prl (rg' {steps = steps rg' + 1})
              else prl (rg' {steps = steps rg' + 1})

argalios :: ResidualGraph -> Int -> ResidualGraph 
argalios rg steps = 
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      es = edges g
      vs = vertices g
      svs = Set.fromList $ map to $ filter (\e -> from e == s) es
      tvs = Set.fromList $ map from $ filter (\e -> to e == t) es
      rg' = prePull $ prePush rg
      olf = netFlow rg
      nfl = netFlow rg'
      steps' = steps + 1
      oovfls = getOverflowing rg
      novfls = getOverflowing rg'
      ovts = Set.intersection (Set.fromList (getOverflowing rg')) tvs
   in if nfl == olf 
         then 
           {-if Set.null ovts || steps > 85-}
           if oovfls == novfls
              then rg' {steps = steps'}
              else argalios rg' steps'
         else argalios rg' steps'

-- | pushes flow though edges with starting vertices which are the ends of source edges 
-- (U) and ending edges that are the start of sink edges (V)
prePush :: ResidualGraph -> ResidualGraph 
prePush rg = 
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      es = edges g
      vs = vertices g
      svs = Set.fromList $ map to $ filter (\e -> from e == s) es
      tvs = Set.fromList $ map from $ filter (\e -> to e == t) es
      uvEdges = filter (\(Edge u v) -> 
        Set.member u svs && Set.member v tvs) es
      pp :: ResidualGraph -> ResidualGraph 
      pp rgr = 
             let !rg' = 
                   {-let !rgr' = steepBFSRelabel rgr-}
                   let !rgr' = bfsRelabel rgr
                       !rgr'' = Set.foldl' (\ac v -> pushNeighbors ac v) rgr' svs
                    in Set.foldl' (\ac v -> pushNeighbors ac v) (rgr'') tvs
                 !ofl = netFlow rgr
                 !nfl = netFlow rg'
              in if nfl == ofl 
                    then rg'
                    else pp rg'
 in pp rg

pushNeighbors :: ResidualGraph -> Vertex -> ResidualGraph
pushNeighbors g v =
  let neimap = netNeighborsMap g
      (fns, rns) = fromJust $ IM.lookup v neimap
      feds = map (\n -> fromTuple (v,n)) fns
      (changed, g') = foldl' (\(ch,ac) e -> 
        let mv = push ac e
         in case mv of 
              Nothing -> (ch,ac)
              Just g'' -> (True,g'')) (False,g) feds
   in g'

prePull :: ResidualGraph -> ResidualGraph 
prePull rg = 
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      es = edges g
      vs = vertices g
      svs = Set.fromList $ map to $ filter (\e -> from e == s) es
      tvs = Set.fromList $ map from $ filter (\e -> to e == t) es
      pp :: ResidualGraph -> ResidualGraph 
      pp rgr = 
         let !rg' = 
               let !rgr' = bfsRelabel rgr
                   !rgr'' = Set.foldl' (\ac v -> pullNeighbors ac v) rgr' tvs
                in Set.foldl' (\ac v -> pullNeighbors ac v) (rgr'') svs
             !ofl = overflowing rgr
             !nfl = overflowing rg'
          in if nfl == ofl 
                then rg'
                else rg'
 in pp rg

pullNeighbors :: ResidualGraph -> Vertex -> ResidualGraph
pullNeighbors g v =
  let neimap = netNeighborsMap g
      (fns, rns) = fromJust $ IM.lookup v neimap
      reds = map (\n -> fromTuple (n,v)) rns
      (changed, g') = foldl' (\(ch,ac) e -> 
        let mv = pull ac e
         in case mv of 
              Nothing -> (ch,ac)
              Just g'' -> (True,g'')) (False,g) reds
   in g'

discharge :: ResidualGraph -> Vertex -> Maybe ResidualGraph
discharge g v =
  let neimap = netNeighborsMap g
      (fns, rns) = fromJust $ IM.lookup v neimap
      feds = map (\n -> fromTuple (v,n)) fns
      reds = map (\n -> fromTuple (n,v)) rns
      !(changed, g') = foldl' (\(ch,ac) e -> 
        let mv = push ac e
         in case mv of 
                Nothing -> (ch,ac)
                Just g'' -> (True,g'')
                ) (False,g) feds
      (changed',g'') = foldl' (\(ch,ac) e -> 
        let mv = pull ac e
         in case mv of 
                Nothing -> (ch,ac)
                Just g''' -> (True,g''')) (changed, g') reds
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
   in case newh <= hv || null neighborHeights of
        True -> Nothing
        False -> Just $ updateHeight g v hv newh

reRelabel :: ResidualGraph -> ResidualGraph
reRelabel rg =
  let es = map snd (IM.toList (netEdges rg))
      g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      vs = vertices g
      sh = numVertices g
   in foldl' (\ ac v -> 
             let (ResidualVertex _ oh ox) = fromJust $ IM.lookup v (netVertices ac)
              in updateHeight ac v oh 0
             ) rg vs 

bfsRelabel :: ResidualGraph -> ResidualGraph
bfsRelabel rg =
  let es = map snd (IM.toList (netEdges rg))
      g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      vs = vertices g
      sh = numVertices g
      (tlvs,slvs) = residualDistances rg
      rg' = foldl' (\ ac (v,l) -> 
             let (ResidualVertex _ oh ox) = fromJust $ IM.lookup v (netVertices ac)
                 h = sh + l
              in updateHeight ac v oh h
                ) rg $ IM.toList slvs 
      rg'' = foldl' (\ ac (v,h) ->
             let (ResidualVertex _ oh ox) = fromJust $ IM.lookup v (netVertices ac)
              in updateHeight ac v oh h
                    ) rg' $ IM.toList tlvs
 in rg'' {steps = steps rg + 1}

steepBFSRelabel :: ResidualGraph -> ResidualGraph
steepBFSRelabel rg =
  let es = netEdges rg
      g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      vs = vertices g
      sh = numVertices g
      revs = saturatedReverseNeighbors (map snd (IM.toList es))
      tlvs = level $ adjBFS revs t
      rg' = foldl' (\ ac (v,h) ->
             let (ResidualVertex _ oh ox) = fromJust $ IM.lookup v (netVertices ac)
              in updateHeight ac v oh h
                     ) rg $ IM.toList tlvs
   in rg'

-- | (distance from sink , distance from source (only those that don't connect
-- to sink))
residualDistances :: ResidualGraph -> (IM.IntMap Int, IM.IntMap Int)
residualDistances rg = 
  let es = map snd (IM.toList $ netEdges rg)
      g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      tres = filter (\(ResidualEdge e c f) -> f < c) es
      tfsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup v ac 
         in case mns of 
               Nothing -> IM.insert v [u] ac
               Just ns -> IM.insert v (u:ns) ac
             ) IM.empty tres
      tbes = filter (\(ResidualEdge e c f) -> f > 0) es
      tsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup v ac 
         in case mns of 
               Nothing -> IM.insert u [v] ac
               Just ns -> IM.insert u (v:ns) ac
             ) tfsatnbs tbes
      {-tlvs = level $ adjBFS tsatnbs t-}

      fvs = map (\v -> (v,())) $ vertices g :: [G.UNode]
      fres = map (\(ResidualEdge e c f) -> toTuple (reverseEdge e)) tres
             ++ map (\(ResidualEdge e c f) -> toTuple e) tbes
      fes = map (\(f,t) -> (f,t,1.0)) (fres) :: [G.LEdge Double]
      mfg = G.mkGraph fvs fes :: I.Gr () Double
      tlvs = IM.fromList $ IBFS.level t mfg

      sres = map (\(ResidualEdge e c f) -> toTuple e) tres
             ++ map (\(ResidualEdge e c f) -> toTuple (reverseEdge e)) tbes
      ses = map (\(f,t) -> (f,t,1.0)) (sres) :: [G.LEdge Double]
      sfg = G.mkGraph fvs ses :: I.Gr () Double
      slvs = IM.fromList $ IBFS.level s sfg
      {-slvs = level $ bfs g s-}
      {-slvs = level $ adjBFS sfsatnbs s-}
    in (tlvs,slvs)

saturatedReverseNeighbors :: [ResidualEdge] -> IM.IntMap [Vertex]
saturatedReverseNeighbors es = 
  let res = filter (\(ResidualEdge e c f) -> f < c) es
      fsatnbs = foldl' (\ac (ResidualEdge e c f) -> 
        let u = from e
            v = to e 
            mns = IM.lookup v ac 
         in case mns of 
               Nothing -> IM.insert v [u] ac
               Just ns -> IM.insert v (u:ns) ac
             ) IM.empty res
  in fsatnbs

updateHeight :: ResidualGraph -> Vertex -> Height -> Height -> ResidualGraph
updateHeight g v oh nh =
  let netvs = netVertices g
      nv = fromJust $ IM.lookup v netvs
      x = excess g v
      ovfs = overflowing g
      s = source $ network g
      t = sink $ network g
      newovfs = 
        if v == s || v == t 
           then ovfs
           else
             let ovfs' = IM.update (\hvs -> 
                         let nhset = Set.delete v hvs
                          in if Set.null nhset
                                      then Nothing 
                                      else Just nhset) oh ovfs
                 mnhset = IM.lookup nh ovfs'
              in if x == 0
                    then ovfs'
                    else
                       case mnhset of 
                         Nothing -> IM.insert nh (Set.singleton v) ovfs'
                         Just nhset -> IM.adjust (Set.insert v) nh ovfs'
  in if v == t || v == s then g
                else g { netVertices = IM.insert v (ResidualVertex v nh x) netvs
                       , overflowing = newovfs
                       } 

updateExcess :: ResidualGraph -> Vertex -> Excess -> ResidualGraph
updateExcess g v nx =
  let netvs = netVertices g
      nv = fromJust $ IM.lookup v netvs
      h = height g v
      ovfs = overflowing g
      s = source $ network g
      t = sink $ network g
      newovfs = 
        if v == s || v == t
           then ovfs
           else
             let ovfs' = IM.update (\hvs -> 
                         let nhset = Set.delete v hvs
                          in if Set.null nhset
                                      then Nothing 
                                      else Just nhset) h ovfs
              in if nx == 0
                then 
                  ovfs'
                else 
                  let mnhset = IM.lookup h ovfs'
                   in case mnhset of 
                        Nothing -> IM.insert h (Set.singleton v) ovfs'
                        Just nhset -> IM.adjust (Set.insert v) h ovfs'
   in if v == t then g
                else g { netVertices = IM.insert v (ResidualVertex v h nx) netvs
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
      reds = map (\n -> fromTuple (v,n)) $ fst ns
   in foldl' (\ac e -> (ac + edgeFlow g e)) 0 reds 
