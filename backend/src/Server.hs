{-# LANGUAGE OverloadedStrings #-}

module Server
( run
)
where

import           System.IO                              (hPutStrLn, stderr)
import           Control.Exception                      (try, SomeException)
import           Network.Wai                            (Middleware)
import qualified Network.Wai.Handler.Warp as Warp       (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import qualified Network.Wai.Middleware.Cors as Wai
import           Servant                                ((:<|>)(..))
import qualified Servant                                (Application, serve, Server)
import qualified Database.PostgreSQL.Simple as Postgres (ConnectInfo(..), connect, Connection)
import qualified API                                    (SudokuApi, sudokuApi)
import qualified Service


run :: IO ()
run = do
  let port = 3001
      settings =
        Warp.setPort port $
        Warp.setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) 
        Warp.defaultSettings
  attempt <- try getDbConnection :: IO (Either SomeException Postgres.Connection)
  case attempt of
    Left _ -> putStrLn "Failed to connect to db, closing"
    Right conn -> Warp.runSettings settings =<< mkApp conn

mkApp :: Postgres.Connection -> IO Servant.Application
mkApp conn = return $ corsWithContentType (Servant.serve API.sudokuApi $ server conn)

server :: Postgres.Connection -> Servant.Server API.SudokuApi
server conn =
  Service.health :<|>
  Service.initGrid :<|>
  Service.getGridById conn :<|>
  Service.saveGrid conn :<|>
  Service.deleteGrid conn :<|>
  Service.solveGrid conn :<|>
  Service.revealCell conn :<|>
  Service.revealGrid conn :<|>
  Service.clearGrid conn

myConnectInfo :: Postgres.ConnectInfo
myConnectInfo = Postgres.ConnectInfo "localhost" 5432 "postgres" "" "sudoku"

getDbConnection :: IO Postgres.Connection
getDbConnection = Postgres.connect myConnectInfo

corsWithContentType :: Middleware
corsWithContentType = Wai.cors (const $ Just policy)
  where policy =
          Wai.simpleCorsResourcePolicy {
            Wai.corsRequestHeaders = ["Content-Type"],
            Wai.corsMethods = "PUT" : Wai.simpleMethods
          }