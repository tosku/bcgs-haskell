import Test.Test
import Test.Grid as G

import Control.Lens

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begin"
  putStrLn $ "Testing Data.Grid"
  let gridTests = G.runTests
  let gts = reportTests gridTests
  mapM_ (\test -> reportTest test) gridTests
  putStrLn $ gts
