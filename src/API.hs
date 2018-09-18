{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API
( SudokuApi
, sudokuApi
)
where

import Servant ((:>), (:<|>)(..), Capture, Get, JSON, Put, ReqBody, Delete,)
import Data.Proxy (Proxy(..))
import Data.Int
import qualified Domain (Grid(..))

-- GET /health
-- GET /grid
-- GET /grid/<id>
-- PUT /grid/<id>
-- DELETE /grid/<id>
-- * PUT /grid/<id>/solve

type SudokuApi =
  "health" :> Get '[JSON] String  :<|>
  "grid" :> Get '[Servant.JSON] Domain.Grid :<|>
  "grid" :> Capture "gridId" Integer :> Get '[JSON] Domain.Grid :<|>
  "grid" :> Capture "gridId" Integer :> ReqBody '[JSON] Domain.Grid :> Put '[JSON] Int64 :<|>
  "grid" :> Capture "gridId" Integer :> Delete '[JSON] Int64 :<|>
  "grid" :> Capture "gridId" Integer :> "solve" :> ReqBody '[JSON] Domain.Grid :> Put '[JSON] Domain.Grid

sudokuApi :: Proxy SudokuApi
sudokuApi = Proxy