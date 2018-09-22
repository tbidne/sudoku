module Service
( health
, getGridById
, initGrid
, saveGrid
, deleteGrid
, solveGrid
)
where

import Prelude hiding (id)
import Servant (err500, Handler, throwError)
import Database.PostgreSQL.Simple as Postgres (Connection)
import Control.Monad.IO.Class (liftIO)
import Data.Int (Int64)
import qualified Domain (Grid(..))
import qualified Database as DB (deleteGrid, getGridById, saveGrid, getCellsByGridId, saveCells)
import qualified Service.Internal as Internal

health :: Handler String
health = return "Sudoku is up!"

initGrid :: Handler Domain.Grid
initGrid = return Internal.blankGrid

getGridById :: Connection -> Integer -> Handler Domain.Grid
getGridById conn id =
  liftIO (DB.getGridById conn id) >>= \gridT ->
  liftIO (DB.getCellsByGridId conn id) >>= \cellTs ->
  let cells = Internal.cellTToCell cellTs in
  let transformed = Internal.gridTToGrid gridT cells in
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

solveGrid :: Connection -> Integer -> Domain.Grid -> Handler Domain.Grid
solveGrid conn id grid =
  let (solved,_) = Internal.solve grid in
  saveGrid conn id solved >>= \_ ->
  return solved