{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.List
import Data.Maybe
import Data.Either.Unwrap
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntSet as Set
import qualified Data.IntMap.Strict as IM

import           Data.Aeson
import Data.Aeson.Text (encodeToLazyText)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.IO as I (appendFile,writeFile)
import           System.Environment
import qualified Data.ByteString.Lazy  as B
import qualified Data.ByteString.Char8 as C
import           GHC.Generics
import           System.IO
import           System.Posix
import           System.Posix.IO.ByteString as BS
import Control.Parallel.Strategies
import qualified Control.Monad.Parallel as MPar

import qualified Data.Graph.Inductive as I
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.MaxFlow as MF
import qualified Data.Graph.Inductive.Query.BFS as IBFS

import Data.Graph
import qualified Data.Graph.Grid as Lat
import qualified Data.BlumeCapel as BC
import Data.BlumeCapel.GSNetwork
import Data.Graph.PushRelabel.Pure
{-import qualified Data.Graph.PushRelabel.STM as IOPR-}

import Data.PRNG
import Data.PRNG.MTRNG

default (Int,Rational,Double)

data ParamRange = ParamRange 
  { rangefrom :: Double
  , rangeto :: Double
  , rangestep :: Double
  } deriving (Show, Generic)
instance FromJSON ParamRange
data JobArguments = JobArguments
  { _l :: Int
  , _d :: Int
  , _delta :: ParamRange
  , _disorder :: ParamRange
  , _disorderType :: String
  , _realizations :: Int
  , _seedofSeeds :: Int
  , _resultfile    :: String
  } deriving (Show, Generic)
instance FromJSON JobArguments

data Observables = Observables
  { energy :: Double
  , magnetization :: Double
  } deriving (Show, Generic)
instance ToJSON Observables

data GSRecord = GSRecord
  { linear_size :: Int
  , dimensions :: Int
  , field :: Double
  , disorder_strength :: Double
  , disorder_type :: String
  , realization_id :: Int
  , observables :: Observables
  } deriving (Show, Generic)
instance ToJSON GSRecord

data GSParams = GSParams
  { l :: Lat.L 
  , d :: Lat.D 
  , r :: BC.DisorderStrength 
  , disorderType :: String 
  , delta :: BC.Delta 
  , seed :: Int
  } deriving (Show,Eq)

getGS :: GSParams -> GroundState
getGS params =
  let distype = if (disorderType params) == "unimodal"
      then BC.Unimodal
      else BC.Dichotomous
      latt = Lat.graphCubicPBC $ Lat.PBCSquareLattice (l params) (d params)
      rbbc = BC.RandomBond { BC.bondDisorder = distype (seed params) (r params)
                        , BC.crystalField = (delta params)
                        }
      real = BC.realization'RBBC rbbc latt
   in groundState real

saveGS :: GSParams -> GroundState -> GSRecord
saveGS !args !gs =
  let nvs = fromIntegral $ (numVertices $ BC.lattice $ BC.realization $ replica gs)
      en = (fromRational $ BC.energy $ replica gs) / nvs :: Double
      mag = (fromRational $ BC.getMagnetization $ BC.configuration $ replica gs) / nvs :: Double
      gsrec = GSRecord
                { linear_size = fromIntegral $ l args
                , dimensions = fromIntegral $ d args
                , field = fromRational $ delta args
                , disorder_strength = fromRational $ r args
                , disorder_type = disorderType args
                , realization_id = seed args
                , observables = Observables 
                  { energy = en
                  , magnetization = mag
                  }
                }
   in gsrec


getRange :: ParamRange -> [Rational]
getRange params =
     let frm = toRational $ rangefrom params
         nxt = frm + toRational (rangestep params)
         rend = toRational $ rangeto params
      in [frm,nxt..rend]

argumentsToParameters :: JobArguments 
                      -> [GSParams]
argumentsToParameters args = 
     let size = (fromIntegral $ _l args) :: Lat.L
         dimensions = fromIntegral $ _d args
         distype = _disorderType args
         rs = getRange $ _disorder args
         deltas = getRange $ _delta args
         seeds = randomInts (getRNG $ _seedofSeeds args :: MTRNG) (_realizations args)
      in map (\(s, (r,d)) -> 
        GSParams { l = size
                 , d = dimensions
                 , r = r
                 , disorderType = distype
                 , delta = d
                 , seed = s
                 }) $ (,) <$> seeds <*> ((,) <$> rs <*> deltas)
         
runJob :: JobArguments -> [GSRecord]
runJob args = do
  let pars = argumentsToParameters args
      gss = parMap rpar (\par -> (par,getGS par)) pars
   in parMap rpar (\(par,gs) -> saveGS par gs) gss

main :: IO ()
main = do
  putStrLn "🏃  started \n"
  args <- getArgs
  let jobfile = args !! 0
  let getJSON = B.readFile jobfile
  readParams <- (eitherDecode <$> getJSON) :: IO (Either String JobArguments)
  (file,recs') <- do
        case readParams of
           Left err -> do
             putStrLn $ "problem with the job file" ++ err
             return ("error",[])
           Right args -> do
             let !recs = runJob args
             let file = show $ _resultfile args
             return (file, recs)
  I.writeFile file (encodeToLazyText recs')
  putStrLn "\n"
  putStrLn "The End!"
