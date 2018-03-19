{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.List

import           System.Environment

import qualified Data.BlumeCapel.GSIO as GSIO


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
      GSIO.readResults resultsfile
      putStrLn "\n"
      putStrLn "The End!"
      return 0 
