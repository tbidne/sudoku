{-# LANGUAGE LambdaCase #-}

module CurlSpec
( spec
)
where 

import Test.Hspec (describe, it, Spec, shouldBe)
import qualified System.Process as Process (callCommand, getPid, Pid, readProcessWithExitCode, spawnCommand)

spec :: Spec
spec = do
  describe "Simple health check" $ do
    it "should curl the application" $ do
      process <- Process.spawnCommand "stack exec sudoku-exe"
      Process.callCommand "sleep 2"
      (errCode, stdout', stderr') <- Process.readProcessWithExitCode "curl" ["localhost:3000/health"] ""
      pid <- Process.getPid process
      cleanup pid
      stdout' `shouldBe` "\"Sudoku is up!\""

-- TODO: replace with System.Process.cleanupProcess once we can upgrade to process 1.6.4.0
cleanup :: Maybe Process.Pid -> IO()
cleanup = do
  \ case
    Nothing -> putStrLn "already closed"
    Just pid -> Process.callCommand $ "kill -15 " ++ show pid