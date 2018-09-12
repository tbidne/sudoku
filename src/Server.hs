{-# LANGUAGE LambdaCase #-}

module Server
( run
)
where

import qualified Network.Wai.Handler.Warp as Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import qualified Servant ((:<|>)(..), Application, err404, Handler, serve, Server, throwError)
import           System.IO
import Table (Table(..))

import qualified API (SudokuApi(..), sudokuApi)

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
  health Servant.:<|>
  getTables Servant.:<|>
  getTableById

health :: Servant.Handler String
health = return "Sudoku is up!"

getTables :: Servant.Handler [Table]
getTables = return [exampleTable]

getTableById :: Integer -> Servant.Handler Table
getTableById = \ case
  0 -> return exampleTable
  _ -> Servant.throwError Servant.err404

exampleTable :: Table
exampleTable = Table 0 "example table"