{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API
( SudokuApi
, sudokuApi
)
where

import Servant ((:>), (:<|>)(..), Capture, Get, JSON, Post, Put, ReqBody, Delete)
import qualified Data.Proxy as Proxy
import qualified Domain (Grid(..))
import Data.Int

-- GET /health
-- GET /grid
-- GET /grid/<id>
-- PUT /grid/<id>
-- DELETE /grid/<id>

type SudokuApi =
  "health" :> Get '[JSON] String :<|>
  "grid" :> Get '[Servant.JSON] Domain.Grid :<|>
  "grid" :> Capture "gridId" Integer :> Get '[JSON] Domain.Grid :<|>
  "grid" :> Capture "gridId" Integer :> ReqBody '[JSON] Domain.Grid :> Put '[JSON] Int64 :<|>
  "grid" :> Capture "gridId" Integer :> Delete '[JSON] Int64

sudokuApi :: Proxy.Proxy SudokuApi
sudokuApi = Proxy.Proxy