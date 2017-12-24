{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Control.Monad.Par.IO
import Control.Monad.IO.Class

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
import qualified Data.BlumeCapel.GSIO as GSIO

import Data.PRNG
import Data.PRNG.MTRNG

default (Int,Rational,Double)

main :: IO ()
main = do
  putStrLn "🏃  started \n"
  putStrLn "reading job file"
  args <- getArgs
  let jobfile = args !! 0
  let getJSON = B.readFile jobfile
  readParams <- (eitherDecode <$> getJSON) :: IO (Either String GSIO.JobArguments)
  case readParams of
       Left err -> do
           putStrLn $ "problem with the job file" ++ err
       Right args -> do
          let file = show $ GSIO._resultfile args
          putStrLn "job file"
          putStrLn $ (GSIO.getJson args)
          case GSIO.argumentsToParameters args of
            Left err -> putStrLn $ "problem with the job file" ++ err
            Right pars -> do
              putStrLn "running"
              let gss = GSIO.runJob pars
              I.writeFile file (encodeToLazyText (GSIO.gsToJSON gss))
              {-putStrLn $ show $ sum $ map (GSIO.magnetization . GSIO.observables) recs-}
              putStrLn "\n"
              putStrLn "The End!"
