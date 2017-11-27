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
import qualified Data.Graph.BFS as BFS

type Capacity = Rational 
type Capacities = M.Map Edge Capacity 
type Flow = Capacity
type Height = Int
type Excess = Capacity
type Level = Int

data ResidualVertex = ResidualVertex Vertex (Level,Level) Height Excess
  deriving (Eq)

instance Show ResidualVertex where
  show (ResidualVertex v l h x) =
    "RVertex " ++ show v ++  " level: " ++
      show l ++ " height: " ++
      show h ++ " excess: " ++
      show (fromRational x :: Double)

data ResidualEdge = ResidualEdge Edge Capacity Flow
  deriving (Eq)

instance Show ResidualEdge where
  show (ResidualEdge e c f) =
    "REdge " ++ show e 
      ++  " " ++
      show (fromRational c :: Double)
      ++  " " ++
      show (fromRational f :: Double)


type ResidualVertices = IM.IntMap ResidualVertex
type ResidualEdges = IM.IntMap ResidualEdge
type NeighborsMap = IM.IntMap ([Vertex], [Vertex])

data Network = Network { graph :: Graph
                       , source :: Vertex
                       , sink :: Vertex
                       , capacities :: Capacities
                       , flow :: Capacities
                       }
                       deriving (Show,Eq)

type Overflowing = IM.IntMap Set.IntSet
data ResidualGraph = ResidualGraph { network :: Network
                                   , netVertices :: ResidualVertices
                                   , netEdges :: ResidualEdges 
                                   , netNeighborsMap :: NeighborsMap 
                                   , foverflowing :: Overflowing
                                   , boverflowing :: Overflowing
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
                    , foverflowing = 
                      let ovfs = getOverflowing vs
                          bfs = BFS.bfs (graph net) (source net)
                          maxLevel = BFS.maxLevel bfs
                          fl v =  let (ResidualVertex _ (l,_) _ _) = fromJust $ IM.lookup v vs
                                   in l
                       in Set.foldl' 
                            (\ac v -> 
                               IM.adjust (\ps -> Set.insert v ps) (fl v) ac
                            ) (IM.fromList (zip [1..maxLevel] (repeat Set.empty))) ovfs
                    , boverflowing = 
                      let ovfs = getOverflowing vs
                          bfs = BFS.bfs (reverseGraph $ graph net) (sink net)
                          maxLevel = BFS.maxLevel bfs
                          bl v =  let (ResidualVertex _ (_,l) _ _) = fromJust $ IM.lookup v vs
                                   in l
                       in Set.foldl' 
                            (\ac v -> 
                               IM.adjust (\ps -> Set.insert v ps) (bl v) ac
                            ) (IM.fromList (zip [1..maxLevel] (repeat Set.empty))) ovfs
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
      vs = vertices $ graph net
      fvs = map (\v -> (v,())) $ vertices g :: [G.UNode]
      es = edges $ graph net
      fes = map (\(Edge f t) -> (f,t,1.0)) es :: [G.LEdge Double]
      mfg = G.mkGraph fvs fes :: I.Gr () Double
      {-flevels = IM.fromList $ IBFS.level s mfg-}
      flevels = BFS.level $ BFS.bfs (graph net) (source net)
      {-blevels = BFS.level $ BFS.bfs (reverseGraph $ graph net) (sink net)-}
      blevels = flevels
      fl v = fromJust $ IM.lookup v flevels
      bl v = fromJust $ IM.lookup v blevels
      zvs = IM.fromList $ zip (vertices g) (map (\v -> if v == t then ResidualVertex t (fl v, bl v) 0 0 else ResidualVertex v (fl v, bl v) 0 0) $ vertices g)
      (sx, nvs) = foldl' (\(cx,ac) (e,c) -> let v = to e
                                             in (cx-c, IM.adjust (const (ResidualVertex v (fl v, bl v) 0 c)) v ac)) (0, zvs) ses
   in IM.insert s (ResidualVertex s (0,sh) sh sx) nvs

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
   {-in map fst $ filter (\nv -> xv nv /= 0 && v /= s && v /= t) xvs-}

pushRelabel :: Network -> IO (Either String ResidualGraph)
pushRelabel net = do
  let initg = initializeResidualGraph net
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
  let ofvs = IM.foldl (\ac ovs -> Set.union ac ovs) Set.empty $ foverflowing res
  let notofvs = filter (\ ov -> 
                          let (ResidualVertex v (l,b) h x) = fromJust (IM.lookup ov (netVertices res)) 
                              ml = (IM.lookup l (foverflowing res)) 
                           in case ml of
                                Nothing -> True
                                Just os -> not $ Set.member ov os
                       ) $ Set.toList $ getOverflowing $ netVertices res
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
                        ++ " overflowings are " ++ show (foverflowing res)
                        ++ " nevertices are " ++ show (netVertices res)

argalios :: ResidualGraph -> Int -> ResidualGraph 
argalios !rg steps = 
  let g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
      es = edges g
      vs = vertices g
      olf = netFlow rg
      rg' = prePush $ prePull rg
      nfl = netFlow rg'
      steps' = steps + 1
      oovfls = foverflowing rg
      novfls = foverflowing rg'
   in if nfl == olf 
         then 
           if oovfls == novfls
              then rg' {steps = steps'}
              else argalios rg' steps'
         else argalios rg' steps'

-- | pushes flow though edges with starting vertices which are the ends of source edges 
-- (U) and ending edges that are the start of sink edges (V)
prePush :: ResidualGraph -> ResidualGraph 
prePush rg = 
  let ovfs = foverflowing rg
   in IM.foldl' (\ac lset -> 
         Set.foldl' (\ac' v -> pushNeighbors ac' v)
         ac lset
      ) rg ovfs

prePull :: ResidualGraph -> ResidualGraph 
prePull rg = 
  let ovfs = boverflowing rg
      !rg' = bfsRelabel rg
   in IM.foldl' (\ac lset -> 
         Set.foldl' (\ac' v -> pullNeighbors ac' v)
         ac lset
                ) rg' ovfs

{-prePull :: ResidualGraph -> ResidualGraph -}
{-prePull rg = -}
  {-let ovfs = foverflowing rg-}
      {-rg' = bfsRelabel rg-}
   {-in IM.foldr (\lset ac -> -}
         {-Set.foldl' (\ac' v -> pullNeighbors ac' v)-}
         {-ac lset-}
               {-) rg' ovfs-}

pushNeighbors :: ResidualGraph -> Vertex -> ResidualGraph
pushNeighbors g v =
  let neimap = netNeighborsMap g
      xv = excess g v
      (fns, rns) = fromJust $ IM.lookup v neimap
      feds = map (\n -> fromTuple (v,n)) fns
   in foldl' (\ac e -> 
                let mv = push ac e
                in case mv of 
                    Nothing -> ac
                    Just g'' -> g'') g feds

pullNeighbors :: ResidualGraph -> Vertex -> ResidualGraph
pullNeighbors g v =
  let neimap = netNeighborsMap g
      (fns, rns) = fromJust $ IM.lookup v neimap
      reds = map (\n -> fromTuple (n,v)) rns
      xv = excess g v
   in foldl' (\ac e -> 
                let mv = pull ac e
                 in case mv of 
                      Nothing -> ac
                      Just g'' -> g'') g reds

discharge :: ResidualGraph -> Vertex -> Maybe ResidualGraph
discharge g v =
  let neimap = netNeighborsMap g
      (fns, rns) = fromJust $ IM.lookup v neimap
      feds = map (\n -> fromTuple (v,n)) fns
      reds = map (\n -> fromTuple (n,v)) rns
      (changed, g') = foldl' (\(ch,ac) e -> 
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
        False -> Just $ updateHeight g v newh

reRelabel :: ResidualGraph -> ResidualGraph
reRelabel rg =
  let g = graph $ network rg
      vs = vertices g
   in foldl' (\ ac v -> updateHeight ac v 0
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
             let h = sh + l
              in updateHeight ac v h
                ) rg $ IM.toList slvs 
      rg'' = foldl' (\ ac (v,h) -> updateHeight ac v h
                    ) rg' $ IM.toList tlvs
 in rg''

-- | (distance from sink , distance from source (only those that don't connect
-- to sink))
residualDistances :: ResidualGraph -> (IM.IntMap Int, IM.IntMap Int)
residualDistances rg = 
  let es = map snd (IM.toList $ netEdges rg)
      g = graph $ network rg
      s = source $ network rg
      t = sink $ network rg
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
      {-fvs = map (\v -> (v,())) $ vertices g :: [G.UNode]-}
      {-fres = map (\(ResidualEdge e c f) -> toTuple (reverseEdge e)) tres-}
             {-++ map (\(ResidualEdge e c f) -> toTuple e) tbes-}
      {-fes = map (\(f,t) -> (f,t,1.0)) (fres) :: [G.LEdge Double]-}
      {-mfg = G.mkGraph fvs fes :: I.Gr () Double-}
      {-tlvs = IM.fromList $ IBFS.level t mfg-}
      {-sres = map (\(ResidualEdge e c f) -> toTuple e) tres-}
             {-++ map (\(ResidualEdge e c f) -> toTuple (reverseEdge e)) tbes-}
      {-ses = map (\(f,t) -> (f,t,1.0)) (sres) :: [G.LEdge Double]-}
      {-sfg = G.mkGraph fvs ses :: I.Gr () Double-}
      {-slvs = IM.fromList $ IBFS.level s sfg-}
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
      (fl,bl) = level g v
      fovfs = foverflowing g
      bovfs = boverflowing g
      s = source $ network g
      t = sink $ network g
      newfovfs = 
        if v == s || v == t
           then fovfs
           else
             let fovfs' = IM.update (\lvs -> 
                         let lset = Set.delete v lvs
                          in if Set.null lset
                                      then Nothing 
                                      else Just lset) fl fovfs
              in if nx == 0
                then 
                  fovfs'
                else 
                  let mlset = IM.lookup fl fovfs'
                   in case mlset of 
                        Nothing -> IM.insert fl (Set.singleton v) fovfs'
                        Just lset -> IM.adjust (Set.insert v) fl fovfs'
      newbovfs = 
        if v == s || v == t
           then bovfs
           else
             let bovfs' = IM.update (\lvs -> 
                         let lset = Set.delete v lvs
                          in if Set.null lset
                                      then Nothing 
                                      else Just lset) bl bovfs
              in if nx == 0
                then 
                  bovfs'
                else 
                  let mlset = IM.lookup bl bovfs'
                   in case mlset of 
                        Nothing -> IM.insert bl (Set.singleton v) bovfs'
                        Just lset -> IM.adjust (Set.insert v) bl bovfs'
   in if v == t then g
                else g { netVertices = IM.insert v (ResidualVertex v (fl,bl) h nx) netvs
                       , foverflowing = newfovfs
                       , boverflowing = newbovfs
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
vertex (ResidualVertex v _ _ _) = v

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

level :: ResidualGraph -> Vertex -> (Level,Level)
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
