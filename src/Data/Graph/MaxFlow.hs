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
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module Data.Graph.MaxFlow
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
import Data.Status
import qualified Data.Vector as V
import qualified Data.Map.Lazy as M
import qualified Data.IntMap.Lazy as IM
import qualified Data.IntSet as Set
import qualified Control.Monad.Parallel as Par
import Control.Concurrent.STM
import Control.Monad

import Data.Graph

type Capacity = Rational 
type Capacities = M.Map Edge Capacity 
type Flow = Capacity
type Height = Natural
type Excess = Capacity
data ResidualVertexP = ResidualVertexP Vertex Height Excess
  deriving (Eq)

data ResidualVertex = ResidualVertex !Vertex !(TVar Height) !(TVar Excess)
  deriving (Eq)

data ResidualEdgeP = ResidualEdgeP Edge Capacity Flow
  deriving (Eq)

data ResidualEdge = ResidualEdge !Edge !Capacity !(TVar Flow)
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
                                   , steps :: Int
                                   }
                       deriving (Eq)

initializeResidualGraph :: Network -> IO ResidualGraph
initializeResidualGraph net = do
  vs <- initializeVertices net
  es <- initializeEdges net
  return ResidualGraph { network = net
                       , netVertices = vs
                       , netEdges = es
                       , netNeighborsMap = getNetNeighborsMap $ graph net
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

initializeVertices :: Network -> IO ResidualVertices
initializeVertices net = do
  let g = graph net
  let cs = capacities net
  let s = source net
  let t = sink net
  let sh = fromIntegral $ numVertices g
  let ses = sourceEdges net
  let zvs = IM.fromList $ zip (vertices g) (map (\v -> ResidualVertexP v 0 0) $ vertices g)
  let (sx, nvs) = foldl' (\(cx,ac) (e,c) -> let v = to e
                                              in (cx-c, IM.insert v (ResidualVertexP v 0 c) ac)) (0, zvs) ses
  let fvs = IM.insert s (ResidualVertexP s sh sx) nvs
  traverse (\(ResidualVertexP v h x) -> do 
                                      th <- newTVarIO h
                                      tx <- newTVarIO x
                                      return $ ResidualVertex v th tx
           ) fvs

initializeEdges :: Network -> IO ResidualEdges
initializeEdges net = do
  let g = graph net
  let cs = capacities net
  let s = source net
  let t = sink net
  let inites = IM.fromList $ map (\(e,c) -> (fromJust $ edgeIndex g e, ResidualEdgeP e c 0)) (M.toList cs)
  let ses = sourceEdges net
  let fes = foldl' (\ac (e,c) -> IM.insert (fromJust $ edgeIndex g e) (ResidualEdgeP e c c) ac) inites ses 
  traverse (\(ResidualEdgeP e c f) -> do
                                      tf <- newTVarIO f
                                      return $ ResidualEdge e c tf
           ) fes

overflowing :: ResidualGraph -> IO [Vertex]
overflowing rg = atomically $ do
  let vs = vertices $ graph $ network rg
  let s = source $ network rg
  let t = sink $ network rg
  xvs <- mapM (\v -> fmap ((,) v) $ excess rg v) vs
  return $ map fst $ filter (\(v,x) -> x/=0 && v /= s && v /= t) xvs

pushRelabel :: Network -> IO Flow
pushRelabel net = do
  initg <- initializeResidualGraph net
  xvs <- overflowing initg
  prl initg xvs 0
  netFlow initg
  where 
      g = graph net
      cs = capacities net
      s = source net
      t = sink net
      vs = vertices g
      ses = sourceEdges net
      prl :: ResidualGraph -> [Vertex] -> Int -> IO ()
      prl rg [] _ = return ()
      prl rg xvs steps = do
        let neimap = netNeighborsMap rg
        Par.mapM_ (\v -> atomically $ 
          do
            let (fns, rns) = fromJust $ IM.lookup v neimap
            !changed <- discharge rg v
            if changed then do
                         return ()
                       else do
                         relabel rg v
             ) xvs
        xvs' <- overflowing rg
        prl rg xvs' (steps+1)

discharge :: ResidualGraph -> Vertex -> STM Bool
discharge g v = do
  let neimap = netNeighborsMap g
  let (fns, rns) = fromJust $ IM.lookup v neimap
  let feds = map (\n -> fromTuple (v,n)) fns
  let reds = map (\n -> fromTuple (n,v)) rns
  !changed <- foldM (\ac e -> fmap ((||) ac) (push g e)) False feds
  !changed' <- foldM (\ac e -> fmap ((||) ac) (pull g e)) changed reds
  return changed'

push :: ResidualGraph -> Edge -> STM Bool
push g e = do 
  let u = from e
  let v = to e
  let t = sink $ network g
  hu <- height g u
  hv <- height g v
  xu <- excess g u 
  xv <- excess g v
  let c = edgeCapacity g e
  f <- edgeFlow g e
  let nvs = netVertices g
  let xf = min xu (c - f)
  if (hu >= hv + 1) && xf > 0 then do
    updateEdge g e (f + xf)
    updateVertex g u hu (xu - xf)
    updateVertex g v hv (xv + xf)
    return True
  else return False

pull :: ResidualGraph -> Edge -> STM Bool
pull g e = do 
  let u = from e
  let v = to e
  hu <- height g u
  hv <- height g v
  xu <- excess g u 
  xv <- excess g v
  let c = edgeCapacity g e
  f <- edgeFlow g e
  let nvs = netVertices g
  let xf = min xv f
  if (hv >= hu + 1)  && xf > 0 then do 
    updateEdge g e (f - xf)
    updateVertex g u hu (xu + xf)
    updateVertex g v hv (xv - xf)
    return True
  else return False

relabel :: ResidualGraph -> Vertex -> STM ()
relabel g v = do
  let ns  = netNeighbors (netNeighborsMap g) v 
  let gnes = map (\n -> fromTuple (v,n)) $ fst ns -- ^ neighboring graph edges
  let rnes = map (\n -> fromTuple (n,v)) $ snd ns -- ^ neighboring reverse edges
  gacfs <- mapM (\e -> fmap ((\f -> (e,(-) (edgeCapacity g e) f))) (edgeFlow g e)) gnes
  let gcfs = filter (\(e,cf) -> cf > 0) gacfs
  racfs <- mapM (\e -> fmap ((\f -> (e,f))) (edgeFlow g e)) rnes
  let rcfs = filter (\(e,cf) -> cf > 0) racfs
  hv <- height g v
  xv <- excess g v
  neighborHeights <- (++) <$> (mapM (\(e,c) -> height g (to e)) gcfs) <*> mapM (\(e,c) -> height g (from e)) rcfs
  let newh = 1 + minimum neighborHeights
  case any (\nh -> hv > nh) neighborHeights || neighborHeights == [] of
     False -> do
       updateVertex g v newh xv
     True -> return ()

updateVertex :: ResidualGraph -> Vertex -> Height -> Excess -> STM ()
updateVertex g v nh nx = do 
  let nv = fromJust $ IM.lookup v (netVertices g)
  if v /= sink (network g) then do
    updateHeight nv nh
    updateExcess nv nx
  else 
    return ()

updateHeight :: ResidualVertex -> Height -> STM ()
updateHeight (ResidualVertex v h x) nh = writeTVar h nh

updateExcess :: ResidualVertex -> Excess -> STM ()
updateExcess (ResidualVertex v h x) nx = writeTVar x nx

updateEdge :: ResidualGraph -> Edge -> Flow -> STM ()
updateEdge g e f = do
  let l = graph $ network g
  let eid = fromJust $ edgeIndex l e
  let es = netEdges g
  let (ResidualEdge oe c tf) = fromJust $ IM.lookup eid es
  writeTVar tf f

netFlow :: ResidualGraph -> IO Flow
netFlow g = atomically $ do
  fl <- fmap negate $ excess g (source (network g))
  return fl

vertex :: ResidualVertex -> Vertex
vertex (ResidualVertex v _ _) = v

height :: ResidualGraph -> Vertex -> STM Height
height g v = do 
  let (ResidualVertex nv h e) = fromJust $ IM.lookup v (netVertices g)
  readTVar h

excess :: ResidualGraph -> Vertex -> STM Excess
excess g v = do
  let (ResidualVertex nv h e) = fromJust $ IM.lookup v (netVertices g)
  readTVar e

edgeCapacity :: ResidualGraph -> Edge -> Capacity
edgeCapacity g e = let (ResidualEdge ne c f) = fromJust $ IM.lookup (fromJust $ edgeIndex (graph $ network g) e) (netEdges g)
                    in c

edgeFlow :: ResidualGraph -> Edge -> STM Flow
edgeFlow g e = do
  let l = graph $ network g
  let (ResidualEdge ne c tf) = fromJust $ IM.lookup (fromJust $ edgeIndex l e) (netEdges g)
  readTVar tf

printVertex :: ResidualGraph -> Vertex -> IO ()
printVertex rg v = do
  (h ,x ,inf ,ouf ) <- atomically $ do
    x' <- excess rg v
    h' <- height rg v
    inf' <- inflow rg v
    ouf' <- outflow rg v
    return (h',x',inf',ouf')
  putStrLn $ show v 
    ++ " h: "++ show h 
    ++ ", exc: " ++ show (fromRational x) 
    ++ ", infl: " ++ show (fromRational inf)
    ++ ", outf: " ++ show (fromRational ouf) 

printEdge :: ResidualGraph -> Edge -> IO ()
printEdge rg e = do
  (c,f) <- atomically $ do
    let c' = edgeCapacity rg e
    f' <- edgeFlow rg e
    return (c',f')
  putStrLn $ show e 
    ++ " c: "++ show (fromRational c) 
    ++ ", f: " ++ show (fromRational f) 

inflow :: ResidualGraph -> Vertex -> STM Flow
inflow g v = do
  let ns  = netNeighbors (netNeighborsMap g) v 
  let reds = map (\n -> fromTuple (n,v)) $ snd ns
  foldM (\ac e -> ((+) ac) <$> edgeFlow g e) 0 reds 

outflow :: ResidualGraph -> Vertex -> STM Flow
outflow g v = do
  let ns  = netNeighbors (netNeighborsMap g) v 
  let feds = map (\n -> fromTuple (v,n)) $ fst ns
  foldM (\ac e -> ((+) ac) <$> edgeFlow g e) 0 feds 

