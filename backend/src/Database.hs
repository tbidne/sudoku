{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database
( deleteGrid
, getGridById
, saveGrid
, saveCell
, getCellsByGridId
, saveCells
, GridT(..)
, CellT(..)
)
where

import Prelude hiding (id)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple (Connection, execute, query, executeMany)
import Database.PostgreSQL.Simple.FromRow (field, fromRow, FromRow)
import Data.Int (Int64)
import qualified Domain (Grid(..), Cell(..))

data GridT =
  GridT {
    gridId :: Int,
    solved :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromRow GridT where
  fromRow = GridT <$> field <*> field

data CellT =
  CellT {
    cellId :: Int,
    parentGridId :: Int,
    row :: Int,
    col :: Int,
    realValue :: Maybe Int,
    userValue :: Maybe Int,
    revealed :: Bool
  }
  deriving (Eq, Show, Generic)

instance FromRow CellT where
  fromRow = CellT <$> field <*> field <*> field <*> field <*> field <*>field <*> field

getGridById :: Connection -> Integer -> IO [GridT]
getGridById conn id = query conn "SELECT * FROM grid WHERE grid.id = ?" [id]

saveGrid :: Connection -> Integer -> Domain.Grid -> IO Int64
saveGrid conn id grid = execute conn "UPDATE grid SET solved = ? WHERE id = ?" (s, id)
  where s = Domain.solved grid

saveCell :: Connection -> Integer -> Domain.Cell -> IO Int64
saveCell conn id cell = execute conn "UPDATE cell SET revealed = ? WHERE id = ?" (r, id)
   where r = Domain.revealed cell

saveCells :: Connection -> [Domain.Cell] -> IO Int64
saveCells conn cells = executeMany conn q vals
  where q = "UPDATE cell c SET user_value = upd.usr, real_value = upd.real, revealed = upd.rvl FROM \
            \(VALUES (?, ?, ?, ?)) as upd(usr, real, id, rvl) WHERE c.id = upd.id"
        vals = map (\c -> (Domain.userValue c, Domain.realValue c, Domain.cellId c, Domain.revealed c)) cells

deleteGrid :: Connection -> Integer -> IO Int64
deleteGrid conn id = execute conn "DELETE FROM grid WHERE id = ?" [id]

getCellsByGridId :: Connection -> Integer -> IO [CellT]
getCellsByGridId conn id = query conn "SELECT * FROM cell WHERE cell.grid_id = ?" [id]