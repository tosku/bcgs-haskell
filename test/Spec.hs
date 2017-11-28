import Test.Test
import Test.Graph.Grid as G
import Test.Graph.BFS as BFS
import Test.BlumeCapel as B
import Test.BlumeCapel.GSNetwork as GSN

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  reportTests $ G.fastTests ++ BFS.fastTests ++ B.fastTests ++ GSN.fastTests 
  {-reportTests $ MF.fastTests ++ B.fastTests ++ GSN.fastTests -}
