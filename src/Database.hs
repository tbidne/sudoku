{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database where

import qualified GHC.Generics as Generics (Generic)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.PostgreSQL.Simple.FromRow as FR (field, fromRow, FromRow)

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
    x :: Int,
    y :: Int,
    realValue :: Int,
    userValue :: Int,
    revealed :: Bool
  }
  deriving (Eq, Show, Generics.Generic)

instance FR.FromRow CellT where
  fromRow = CellT <$> FR.field <*> FR.field <*> FR.field <*> FR.field <*> FR.field <*> FR.field <*> FR.field

allGrids :: Postgres.Connection -> IO [GridT]
allGrids c = Postgres.query_ c "SELECT * FROM grid"

allCells :: Postgres.Connection -> IO [CellT]
allCells c = Postgres.query_ c "SELECT * FROM cell"

myConnectInfo :: Postgres.ConnectInfo
myConnectInfo = Postgres.ConnectInfo "localhost" 5432 "postgres" "" "sudoku"

getConnection :: IO Postgres.Connection
getConnection = Postgres.connect myConnectInfo