module Server
( run
)
where

import qualified Network.Wai.Handler.Warp as Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import qualified Servant ((:<|>)(..), Application, serve, Server)
import           System.IO
import qualified Service (health, getGrids, getGridById, initGrid)

import qualified API (SudokuApi, sudokuApi)

run :: IO ()
run = do
  let port = 3000
      settings =
        Warp.setPort port $
        Warp.setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) 
        Warp.defaultSettings
  Warp.runSettings settings =<< mkApp

mkApp :: IO Servant.Application
mkApp = return $ Servant.serve API.sudokuApi server

server :: Servant.Server API.SudokuApi
server =
  Service.health Servant.:<|>
  Service.initGrid Servant.:<|>
  Service.getGrids Servant.:<|>
  Service.getGridById