{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}

module Main where

import Data.List

import           System.Environment

import qualified Data.BlumeCapel.GSIO as GSIO


main :: IO ()
main = do
  putStrLn "🏃  started \n"
  putStrLn "reading job file"
  args <- getArgs
  let jobfile = args !! 0
  GSIO.runJob jobfile
  putStrLn "\n"
  putStrLn "The End!"
