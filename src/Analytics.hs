{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Either
import           Data.List

import           System.Environment

import qualified Data.BlumeCapel.GSIO       as GSIO
import qualified Data.BlumeCapel.Statistics as ST


main :: IO Int
main = do
  args <- getArgs
  case null args of
    True -> do
      putStrLn "No results file entered"
      return 1
    False -> do
      putStrLn "Reading results file \n"
      let resultsfile = args !! 0
      gss <- GSIO.readResults resultsfile
      let stats = ST.gsStats gss
      putStrLn $ show stats
      putStrLn $ show $ ST.size stats
      putStrLn "The End!"
      return 0
