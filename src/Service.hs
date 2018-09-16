module Service
( health
, getGridById
, initGrid
, saveGrid
, deleteGrid
)
where

import qualified Servant (err404, err500, Handler, throwError)
import qualified Domain (Cell(..), Grid(..))
import qualified Database.PostgreSQL.Simple as Postgres (Connection)
import qualified Database as DB (deleteGrid, getGridById, saveGrid, getCellsByGridId, GridT(..), CellT(..))
import Control.Monad.IO.Class
import Data.Int

-- API

health :: Servant.Handler String
health = return "Sudoku is up!"

initGrid :: Servant.Handler Domain.Grid
initGrid = return blankGrid

getGridById :: Postgres.Connection -> Integer -> Servant.Handler Domain.Grid
getGridById conn id = do
  gridT <- liftIO $ DB.getGridById conn id
  cellTs <- liftIO $ DB.getCellsByGridId conn id
  let cells = cellTToCell cellTs []
  let transformed = gridTToGrid gridT cells
  case transformed of
    Nothing -> Servant.throwError Servant.err500
    Just grid -> return grid

saveGrid :: Postgres.Connection -> Integer -> Domain.Grid -> Servant.Handler Int64
saveGrid conn id grid = liftIO $ DB.saveGrid conn id grid

deleteGrid :: Postgres.Connection -> Integer -> Servant.Handler Int64
deleteGrid conn id = liftIO $ DB.deleteGrid conn id

-- Internal

blankGrid :: Domain.Grid
blankGrid = Domain.Grid 0 cells False
  where cells = replicate 81 exampleCell

exampleGrid :: Domain.Grid
exampleGrid = Domain.Grid 0 [] False

exampleCell :: Domain.Cell
exampleCell = Domain.Cell 0 0 0 0 0 False

-- query should return a single object
gridTToGrid :: [DB.GridT] -> Maybe [Domain.Cell] -> Maybe Domain.Grid
gridTToGrid _ Nothing = Nothing
gridTToGrid [] _ = Nothing
gridTToGrid [gridT] mCells = case mCells of
  Nothing -> Nothing
  Just cells -> Just $ Domain.Grid id cells solved
  where id = DB.gridId gridT
        solved = DB.solved gridT
gridTToGrid (gridT:_) _ = Nothing

-- TODO use fold here
-- query should return a single object
cellTToCell :: [DB.CellT] -> [Domain.Cell] -> Maybe [Domain.Cell]
cellTToCell [] [] = Nothing
cellTToCell [] acc = Just acc
cellTToCell (x:xs) acc = cellTToCell xs $ acc ++ [Domain.Cell id row col realValue userValue revealed]
  where id = DB.cellId x
        row = DB.row x
        col = DB.col x
        realValue = DB.realValue x
        userValue = DB.userValue x
        revealed = DB.revealed x