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

health :: Handler String
health = return "Sudoku is up!"

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

solveGrid :: Connection -> Integer -> Domain.Grid -> Handler Domain.Grid
solveGrid conn id grid =
  case Internal.solve grid of
    Nothing ->
      liftIO (putStrLn "Could not solve grid!") >> throwError err500
    Just solved -> 
      saveGrid conn id solved >> return solved

revealCell :: Connection -> Integer -> Domain.Cell -> Handler Domain.Cell
revealCell conn id cell =
  let revealed = Internal.revealCell cell in
  liftIO (DB.saveCell conn id revealed) >> return revealed

revealGrid :: Connection -> Integer -> Domain.Grid -> Handler Domain.Grid
revealGrid conn id grid =
  let revealed = Internal.revealGrid grid in
  saveGrid conn id revealed >> return revealed

clearGrid :: Connection -> Integer -> Handler Domain.Grid
clearGrid conn id =
  let newGrid = Internal.blankGrid in
  saveGrid conn id newGrid >> return newGrid