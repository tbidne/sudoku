module Server
( run
)
where

import qualified Network.Wai.Handler.Warp as Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import Servant ((:<|>)(..))
import qualified Servant (Application, serve, Server)
import           System.IO
import qualified Service (health, getGridById, initGrid, saveGrid, deleteGrid)
import qualified Database.PostgreSQL.Simple as Postgres (connect, ConnectInfo(..), Connection)
import qualified API (SudokuApi, sudokuApi)
import Control.Exception (try, SomeException)

run :: IO ()
run = do
  let port = 3000
      settings =
        Warp.setPort port $
        Warp.setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) 
        Warp.defaultSettings
  attempt <- try getDbConnection :: IO (Either SomeException Postgres.Connection)
  case attempt of
    Left ex -> putStrLn "Failed to connect to db, closing"
    Right conn -> Warp.runSettings settings =<< mkApp conn

mkApp :: Postgres.Connection -> IO Servant.Application
mkApp conn = return $ Servant.serve API.sudokuApi $ server conn

server :: Postgres.Connection -> Servant.Server API.SudokuApi
server conn =
  Service.health :<|>
  Service.initGrid :<|>
  Service.getGridById conn :<|>
  Service.saveGrid conn :<|>
  Service.deleteGrid conn

myConnectInfo :: Postgres.ConnectInfo
myConnectInfo = Postgres.ConnectInfo "localhost" 5432 "postgres" "" "sudoku"

getDbConnection :: IO Postgres.Connection
getDbConnection = Postgres.connect myConnectInfo