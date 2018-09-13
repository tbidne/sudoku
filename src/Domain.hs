{-# LANGUAGE DeriveGeneric #-}

module Domain
( Table(..)
, Cell(..)
)
where

import qualified GHC.Generics as Generics (Generic)
import qualified Data.Aeson as Aeson (FromJSON, ToJSON)

data Table
  = Table {
    tableId :: Integer,
    cells :: [[Cell]],
    isSolved :: Bool
  }
  deriving (Eq, Show, Generics.Generic)

instance Aeson.ToJSON Table
instance Aeson.FromJSON Table

data Cell
  = Cell {
    id :: Integer,
    row :: Int,
    col :: Int,
    realValue :: Int,
    userValue :: Int,
    isRevealed :: Bool
  }
  deriving (Eq, Show, Generics.Generic)

instance Aeson.ToJSON Cell
instance Aeson.FromJSON Cell