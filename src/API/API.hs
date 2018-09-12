{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module API.API where

import qualified Data.Aeson as Aeson (FromJSON, ToJSON)
import qualified GHC.Generics as Generics (Generic)
import qualified Network.Wai.Handler.Warp as Warp (defaultSettings, runSettings, setBeforeMainLoop, setPort)
import qualified Servant ((:>)(..), (:<|>)(..), Application, Capture, err404, Get,
  Handler, JSON, Post, Proxy, serve, Server, throwError)
import qualified Data.Proxy as Proxy
import           System.IO

-- * api

type TableApi =
  "table" Servant.:> Servant.Get '[Servant.JSON] [Table] Servant.:<|>
  "table" Servant.:> Servant.Capture "tableId" Integer Servant.:> Servant.Get '[Servant.JSON] Table

tableApi :: Proxy.Proxy TableApi
tableApi = Proxy.Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        Warp.setPort port $
        Warp.setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) 
        Warp.defaultSettings
  Warp.runSettings settings =<< mkApp

mkApp :: IO Servant.Application
mkApp = return $ Servant.serve tableApi server

server :: Servant.Server TableApi
server =
  getTables Servant.:<|>
  getTableById

getTables :: Servant.Handler [Table]
getTables = return [exampleTable]

getTableById :: Integer -> Servant.Handler Table
getTableById = \ case
  0 -> return exampleTable
  _ -> Servant.throwError Servant.err404

exampleTable :: Table
exampleTable = Table 0 "example table"

-- * table

data Table
  = Table {
    tableId :: Integer,
    tableText :: String
  }
  deriving (Eq, Show, Generics.Generic)

instance Aeson.ToJSON Table
instance Aeson.FromJSON Table