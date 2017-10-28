{-# LANGUAGE TemplateHaskell #-}
module Test.Test 
    ( Test
    , runTest
    , reportTests
    , testPassed
    , testFailed
    ) where

import Language.Haskell.TH
import Control.Lens
import Data.Tuple
import System.Console.ANSI
import System.Exit

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
  
runTest :: Test -> IO Test
runTest t = do
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
      putStr $ ": " ++ succe 
      setSGR [SetColor Foreground Vivid Green]
      putStrLn " ✓"
      setSGR [Reset]
  return t

reportTests :: [Test] -> IO ()
reportTests ts = do
  tests <- sequence $ map runTest ts
  let lt = length tests
  let passedtests = filter 
                    (\test -> case test ^. outcome of 
                    Left _ -> False
                    Right _ -> True)
                    tests
  let failedTests = lt - length passedtests
  let passedAll = length passedtests == lt
  case passedAll of
       True -> do
         putStrLn $ "Passed all " ++ (show lt) ++ " tests!! 🎉"
       False -> do
         putStrLn $ "Failed "  ++ (show failedTests) ++ " test(s) 😣"
         exitFailure
