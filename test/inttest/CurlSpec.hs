module CurlSpec
( spec
)
where 

import Test.Hspec (describe, it, Spec, shouldBe)
import System.Process (callCommand, getPid, Pid, readProcessWithExitCode, spawnCommand)
import qualified Server (run)

spec :: Spec
spec = do
  describe "Simple health check" $ do
    it "should curl the application" $ do
      process <- spawnCommand "stack exec sudoku-exe"
      callCommand "sleep 1"
      (errCode, stdout', stderr') <- readProcessWithExitCode "curl" ["localhost:3000/health"] ""
      pid <- getPid process
      cleanup pid
      stdout' `shouldBe` "\"Sudoku is up!\""

-- TODO: replace with System.Process.cleanupProcess once we can upgrade to process 1.6.4.0
cleanup :: Maybe Pid -> IO()
cleanup x = do
  case x of
    Nothing -> putStrLn "already closed"
    Just pid -> callCommand $ "kill -15 " ++ show pid