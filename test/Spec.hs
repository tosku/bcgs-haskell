import qualified TestHS as T
import Test.BlumeCapel as B
import Test.BlumeCapel.GSNetwork as GSN
import Test.BlumeCapel.GSIO as GSIO

main :: IO ()
main = do
  putStrLn "\n"
  putStrLn $ "Test Begins"
  T.reportTests $ B.fastTests 
    ++ GSN.fastTests  
    ++ GSIO.fastTests
