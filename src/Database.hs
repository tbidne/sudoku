{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database
( deleteGrid
, getGridById
, saveGrid
, getCellsByGridId
, GridT(..)
, CellT(..)
)
where

import Prelude hiding (id)
import qualified GHC.Generics as Generics (Generic)
import qualified Database.PostgreSQL.Simple as Postgres (Connection, execute, query)
import qualified Database.PostgreSQL.Simple.FromRow as FR (field, fromRow, FromRow)
import qualified Domain (Grid(..))
import Data.Int

data GridT =
  GridT {
    gridId :: Int,
    solved :: Bool
  }
  deriving (Eq, Show, Generics.Generic)

instance FR.FromRow GridT where
  fromRow = GridT <$> FR.field <*> FR.field

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
  deriving (Eq, Show, Generics.Generic)

instance FR.FromRow CellT where
  fromRow = CellT <$> FR.field <*> FR.field <*> FR.field <*> FR.field <*> FR.field <*> FR.field <*> FR.field

getGridById :: Postgres.Connection -> Integer -> IO [GridT]
getGridById conn id = Postgres.query conn "SELECT * FROM grid WHERE grid.id = ?" [id]

saveGrid :: Postgres.Connection -> Integer -> Domain.Grid -> IO Int64
saveGrid conn id grid = Postgres.execute conn "UPDATE grid SET solved = ? WHERE id = ?" (s, id)
  where s = Domain.isSolved grid

deleteGrid :: Postgres.Connection -> Integer -> IO Int64
deleteGrid conn id = Postgres.execute conn "DELETE FROM grid WHERE id = ?" [id]

getCellsByGridId :: Postgres.Connection -> Integer -> IO [CellT]
getCellsByGridId conn id = Postgres.query conn "SELECT * FROM cell WHERE cell.grid_id = ?" [id]