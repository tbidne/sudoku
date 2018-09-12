{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API
( SudokuApi(..)
, sudokuApi
)
where

import qualified Servant ((:>)(..), (:<|>)(..), Capture, Get, JSON, Post, Proxy)
import qualified Data.Proxy as Proxy
import           System.IO

import Table (Table(..))

type SudokuApi =
  "health" Servant.:> Servant.Get '[Servant.JSON] String Servant.:<|>
  "table" Servant.:> Servant.Get '[Servant.JSON] [Table] Servant.:<|>
  "table" Servant.:> Servant.Capture "tableId" Integer Servant.:> Servant.Get '[Servant.JSON] Table

sudokuApi :: Proxy.Proxy SudokuApi
sudokuApi = Proxy.Proxy

--