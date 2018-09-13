{-# LANGUAGE LambdaCase #-}

module Service
( health
, initTable
, getTables
, getTableById
)
where

import qualified Servant (err404, Handler, throwError)
import qualified Domain (Cell(..), Table(..))

health :: Servant.Handler String
health = return "Sudoku is up!"

initTable :: Servant.Handler Domain.Table
initTable = return blankTable

getTables :: Servant.Handler [Domain.Table]
getTables = return [exampleTable]

getTableById :: Integer -> Servant.Handler Domain.Table
getTableById = \ case
  0 -> return exampleTable
  _ -> Servant.throwError Servant.err404

blankTable :: Domain.Table
blankTable = Domain.Table 0 cells False
  where cells = (replicate 9 . replicate 9) exampleCell

exampleTable :: Domain.Table
exampleTable = Domain.Table 0 [] False

exampleCell :: Domain.Cell
exampleCell = Domain.Cell 0 0 0 0 0 False