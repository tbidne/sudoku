{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API
( SudokuApi
, sudokuApi
)
where

import qualified Servant ((:>), (:<|>)(..), Capture, Get, JSON, Post)
import qualified Data.Proxy as Proxy


import qualified Domain (Table(..))

type SudokuApi =
  "health" Servant.:> Servant.Get '[Servant.JSON] String Servant.:<|>
  "table" Servant.:> Servant.Post '[Servant.JSON] Domain.Table Servant.:<|>
  "table" Servant.:> Servant.Get '[Servant.JSON] [Domain.Table] Servant.:<|>
  "table" Servant.:> Servant.Capture "tableId" Integer Servant.:> Servant.Get '[Servant.JSON] Domain.Table

sudokuApi :: Proxy.Proxy SudokuApi
sudokuApi = Proxy.Proxy