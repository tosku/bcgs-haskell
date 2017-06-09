{-# LANGUAGE TemplateHaskell #-}
module Test.Test 
    ( Test
    , reportTest
    , reportTests
    , testPassed
    , testFailed
    ) where

import Language.Haskell.TH
import Control.Lens
import Data.Grid
import Data.Tuple
import Data.Lattice
import System.Console.ANSI

data Test = Test
  { _name :: String
  , _outcome :: Either (String, String) String
  } deriving (Show, Eq)
makeLenses ''Test

testPassed :: String -> String -> Test
testPassed t s = Test 
  { _name = t
  , _outcome = Right s
  }
  
testFailed :: String -> (String,String) -> Test
testFailed t f = Test 
  { _name = t
  , _outcome = Left f
  }
  
reportTest :: Test -> IO ()
reportTest t = do
  case t ^. outcome of
    Left err -> do
      putStr $ t ^. name 
      putStr $ " expected: " ++ view _1 err
      putStr $ " got: " ++ view _2 err
      setSGR [SetColor Foreground Vivid Red]
      putStrLn  " ✗" 
      setSGR [Reset]
    Right succe -> do
      putStr $ t ^. name 
      setSGR [SetColor Foreground Vivid Green]
      putStrLn " ✓"
      setSGR [Reset]

reportTests :: [Test] -> String
reportTests ts = do
  let lt = length ts
  let passedtests = filter 
                    (\test -> case test ^. outcome of 
                    Left _ -> False
                    Right _ -> True)
                    ts
  let failedTests = lt - length passedtests
  let passedAll = length passedtests == lt
  let report = case passedAll of
               True -> "Passed all " ++ (show lt) ++ "!"
               False -> "Failed "  ++ (show failedTests)
  show report
