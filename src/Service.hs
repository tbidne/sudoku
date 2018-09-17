module Service
( health
, getGridById
, initGrid
, saveGrid
, deleteGrid
)
where

import Prelude hiding (id)
import Servant (err500, Handler, throwError)
import Database.PostgreSQL.Simple as Postgres (Connection)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import qualified Domain (Cell(..), Grid(..))
import qualified Database as DB (deleteGrid, getGridById, saveGrid, getCellsByGridId, saveCells)
import qualified Database as DB_Cell (CellT(..))
import qualified Database as DB_Grid (GridT(..))

-- API

health :: Handler String
health = return "Sudoku is up!"

initGrid :: Handler Domain.Grid
initGrid = return blankGrid

getGridById :: Connection -> Integer -> Handler Domain.Grid
getGridById conn id =
  liftIO (DB.getGridById conn id) >>= \gridT ->
  liftIO (DB.getCellsByGridId conn id) >>= \cellTs ->
  let cells = cellTToCell cellTs [] in
  let transformed = gridTToGrid gridT cells in
  case transformed of
    Nothing -> throwError err500
    Just grid -> return grid

saveGrid :: Connection -> Integer -> Domain.Grid -> Handler Int64
saveGrid conn id grid =
  liftIO (DB.saveGrid conn id grid) >>= \x ->
  liftIO (DB.saveCells conn $ Domain.cells grid) >>= \y ->
  return (x + y)

deleteGrid :: Connection -> Integer -> Handler Int64
deleteGrid conn id = liftIO $ DB.deleteGrid conn id

-- Internal

blankGrid :: Domain.Grid
blankGrid = Domain.Grid 0 cells False
  where cells = replicate 81 exampleCell

exampleCell :: Domain.Cell
exampleCell = Domain.Cell 0 0 0 0 0 False

-- query should return a single object
gridTToGrid :: [DB_Grid.GridT] -> Maybe [Domain.Cell] -> Maybe Domain.Grid
gridTToGrid _ Nothing = Nothing
gridTToGrid [] _ = Nothing
gridTToGrid [gridT] mCells =
  case mCells of
    Nothing -> Nothing
    Just cells -> Just $ Domain.Grid id cells solved
    where id = DB_Grid.gridId gridT
          solved = DB_Grid.solved gridT
gridTToGrid _ _ = Nothing

-- TODO use fold here
-- query should return a single object
cellTToCell :: [DB_Cell.CellT] -> [Domain.Cell] -> Maybe [Domain.Cell]
cellTToCell [] [] = Nothing
cellTToCell [] acc = Just acc
cellTToCell (x:xs) acc = cellTToCell xs $ acc ++ [Domain.Cell id row col realValue userValue revealed]
  where id = DB_Cell.cellId x
        row = DB_Cell.row x
        col = DB_Cell.col x
        realValue =  fromMaybe (-1) $ DB_Cell.realValue x
        userValue =  fromMaybe (-1) $ DB_Cell.userValue x
        revealed = DB_Cell.revealed x