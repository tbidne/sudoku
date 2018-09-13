{-# LANGUAGE LambdaCase #-}

module Service
( health
, initGrid
, getGrids
, getGridById
)
where

import qualified Servant (err404, Handler, throwError)
import qualified Domain (Cell(..), Grid(..))

health :: Servant.Handler String
health = return "Sudoku is up!"

initGrid :: Servant.Handler Domain.Grid
initGrid = return blankGrid

getGrids :: Servant.Handler [Domain.Grid]
getGrids = return [exampleGrid]

getGridById :: Integer -> Servant.Handler Domain.Grid
getGridById = \ case
  0 -> return exampleGrid
  _ -> Servant.throwError Servant.err404

blankGrid :: Domain.Grid
blankGrid = Domain.Grid 0 cells False
  where cells = (replicate 9 . replicate 9) exampleCell

exampleGrid :: Domain.Grid
exampleGrid = Domain.Grid 0 [] False

exampleCell :: Domain.Cell
exampleCell = Domain.Cell 0 0 0 0 0 False