import Test.Test
import Test.Grid as G
import Test.BlumeCapel as B

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  reportTests $ G.fastTests ++ B.fastTests
