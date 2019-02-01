{-|
Module      : Service
Description : The main server functions
License     : MIT
Maintainer  : tbidne@gmail.com

Contains the main web server functions.
-}
module Service
( health
, getGridById
, saveGrid
, solveGrid
, revealCell
, revealGrid
, clearGrid
)
where

import           Prelude                         hiding (id)
import           Servant                                (err500, Handler, throwError)
import           Database.PostgreSQL.Simple as Postgres (Connection)
import           Control.Monad.IO.Class                 (liftIO)
import           Data.Int                               (Int64)
import qualified Domain                                 (Cell(..), Grid(..))
import qualified Database as DB
import qualified Service.Internal as Internal

-- | Simple health check. Returns a 'Handler' "Sudoku is up!"
health :: Handler String
health = return "Sudoku is up!"

-- | For a 'Connection' and 'Integer' @gridId@, returns a 'Handler' 'Domain.Grid'.
-- Throws 500 if an error is encountered.
getGridById :: Connection -> Integer -> Handler Domain.Grid
getGridById conn id = do
  gridT <- liftIO $ DB.getGridById conn id
  cellTs <- liftIO $ DB.getCellsByGridId conn id
  let cells = Internal.cellTToCell cellTs
  case Internal.gridTToGrid gridT cells of
    Nothing -> throwError err500
    Just grid -> return grid

-- | For a 'Connection', 'Integer' @gridId@, and 'Domain.Grid', saves the grid
-- and returns the total saved objects (Grid + cells).
saveGrid :: Connection -> Integer -> Domain.Grid -> Handler Int64
saveGrid conn id grid = do
  savedGrid <- liftIO $ DB.saveGrid conn id grid
  savedCells <- liftIO $ DB.saveCells conn $ Domain.cells grid
  return $ savedGrid + savedCells

-- | For a 'Connection', 'Integer' @gridId@, and 'Domain.Grid', attempts to solve
-- the grid. If the grid cannot be solved an erro message is printed to the
-- console and 500 is thrown.
solveGrid :: Connection -> Integer -> Domain.Grid -> Handler Domain.Grid
solveGrid conn id grid =
  case Internal.solve grid of
    Nothing ->
      liftIO (putStrLn "Could not solve grid!") >> throwError err500
    Just solved ->
      saveGrid conn id solved >> return solved

-- | For a 'Connection', 'Integer' @cellId@, and 'Domain.Cell', reveals
-- a cell.
revealCell :: Connection -> Integer -> Domain.Cell -> Handler Domain.Cell
revealCell conn id cell =
  let revealed = Internal.revealCell cell in
  liftIO (DB.saveCell conn id revealed) >> return revealed

-- | For a 'Connection', 'Integer' @gridId@, and 'Domain.Grid', reveals
-- the grid.
revealGrid :: Connection -> Integer -> Domain.Grid -> Handler Domain.Grid
revealGrid conn id grid =
  let revealed = Internal.revealGrid grid in
  saveGrid conn id revealed >> return revealed

-- | For a 'Connection', 'Integer' @gridId@, and 'Domain.Grid', clears
-- the grid.
clearGrid :: Connection -> Integer -> Handler Domain.Grid
clearGrid conn id =
  let newGrid = Internal.blankGrid in
  saveGrid conn id newGrid >> return newGrid