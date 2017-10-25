import Test.Test
import Test.Graph.BFS as BFS
import Test.Grid as G
import Test.BlumeCapel as B
import Test.BlumeCapel.MaxFlow as F

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  reportTests $ G.fastTests ++ B.fastTests ++ F.fastTests ++ BFS.fastTests
