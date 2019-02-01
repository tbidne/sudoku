{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

{-|
Module      : API
Description : Sudoku REST API
License     : MIT
Maintainer  : tbidne@gmail.com

Contains the REST API for Sudoku.
-}
module API
( SudokuApi
, sudokuApi
)
where

import           Servant    ((:>), (:<|>), Capture, Get, JSON, Put, ReqBody)
import           Data.Proxy (Proxy(..))
import           Data.Int   (Int64)
import qualified Domain     (Cell(..), Grid(..))

-- | __GET__: @/health@. Returns 'JSON'.
--
-- __GET__: @\/grid\/\<id\>@. Returns 'JSON' 'Domain.Grid' for the given @id@.
--
-- __PUT__: @\/grid\/\<id\>@. For given 'JSON' 'ReqBody' `Domain.Grid` with @id@,
-- saves the grid and returns JSON @Int64@ representing number
-- of saved cells + saved grid.
--
-- __PUT__: @\/grid\/\<id\>/solve@, For given JSON ReqBody `Domain.Grid` with @id@, solves and returns the grid.
--
-- __PUT__: @\/cell\/\<id\>\/reveal@, For given JSON ReqBody `Domain.Cell` with @id@, reveals the cell.
--
-- __PUT__: @\/grid\/\<id\>\/reveal@, For given JSON ReqBody `Domain.Grid` with @id@, reveals the grid.
--
-- __PUT__: @\/grid\/\<id\>\/clear@, For given gridId @id@, clears the grid.
type SudokuApi =
  "health" :> Get '[JSON] String :<|>
  "grid" :> Capture "gridId" Integer :> Get '[JSON] Domain.Grid :<|>
  "grid" :> Capture "gridId" Integer :> ReqBody '[JSON] Domain.Grid :> Put '[JSON] Int64 :<|>
  "grid" :> Capture "gridId" Integer :> "solve" :> ReqBody '[JSON] Domain.Grid :> Put '[JSON] Domain.Grid :<|>
  "cell" :> Capture "cellId" Integer :> "reveal" :> ReqBody '[JSON] Domain.Cell :> Put '[JSON] Domain.Cell :<|>
  "grid" :> Capture "gridId" Integer :> "reveal" :> ReqBody '[JSON] Domain.Grid :> Put '[JSON] Domain.Grid :<|>
  "grid" :> Capture "gridId" Integer :> "clear" :> Put '[JSON] Domain.Grid

-- | Returns 'SudokuApi' behind 'Proxy'.
sudokuApi :: Proxy SudokuApi
sudokuApi = Proxy