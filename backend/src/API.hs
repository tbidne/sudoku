{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API
( SudokuApi
, sudokuApi
)
where

import           Servant    ((:>), (:<|>), Capture, Get, JSON, Put, ReqBody)
import           Data.Proxy (Proxy(..))
import           Data.Int   (Int64)
import qualified Domain     (Cell(..), Grid(..))

-- GET /health
-- GET /grid/<id>
-- PUT /grid/<id>, Grid
-- PUT /grid/<id>/solve, Grid
-- PUT: cell/<id>/reveal, Cell
-- PUT: grid/<id>/reveal, Grid
-- PUT: grid/<id>/clear

type SudokuApi =
  "health" :> Get '[JSON] String :<|>
  "grid" :> Capture "gridId" Integer :> Get '[JSON] Domain.Grid :<|>
  "grid" :> Capture "gridId" Integer :> ReqBody '[JSON] Domain.Grid :> Put '[JSON] Int64 :<|>
  "grid" :> Capture "gridId" Integer :> "solve" :> ReqBody '[JSON] Domain.Grid :> Put '[JSON] Domain.Grid :<|>
  "cell" :> Capture "cellId" Integer :> "reveal" :> ReqBody '[JSON] Domain.Cell :> Put '[JSON] Domain.Cell :<|>
  "grid" :> Capture "gridId" Integer :> "reveal" :> ReqBody '[JSON] Domain.Grid :> Put '[JSON] Domain.Grid :<|>
  "grid" :> Capture "gridId" Integer :> "clear" :> Put '[JSON] Domain.Grid

sudokuApi :: Proxy SudokuApi
sudokuApi = Proxy