module Service
( health
, getGridById
, initGrid
, saveGrid
, deleteGrid
)
where

import qualified Servant (err404, Handler, throwError)
import qualified Domain (Cell(..), Grid(..))
import qualified Database.PostgreSQL.Simple as Postgres (Connection)
import qualified Database as DB (deleteGrid, getGridById, saveGrid, GridT(..))
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
  let transformed = gridToGridT gridT
  case transformed of
    Nothing -> Servant.throwError Servant.err404
    Just t -> return t

saveGrid :: Postgres.Connection -> Integer -> Domain.Grid -> Servant.Handler Int64
saveGrid conn id grid = liftIO $ DB.saveGrid conn id grid

deleteGrid :: Postgres.Connection -> Integer -> Servant.Handler Int64
deleteGrid conn id = liftIO $ DB.deleteGrid conn id

-- Internal

blankGrid :: Domain.Grid
blankGrid = Domain.Grid 0 cells False
  where cells = (replicate 9 . replicate 9) exampleCell

exampleGrid :: Domain.Grid
exampleGrid = Domain.Grid 0 [] False

exampleCell :: Domain.Cell
exampleCell = Domain.Cell 0 0 0 0 0 False

gridToGridT :: [DB.GridT] -> Maybe Domain.Grid
gridToGridT [] = Nothing
gridToGridT (gridT:xs) = Just $ Domain.Grid id cells solved
  where id = DB.gridId gridT
        cells = []
        solved = DB.solved gridT