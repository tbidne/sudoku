{-# LANGUAGE DeriveGeneric #-}

module Domain
( Grid(..)
, Cell(..)
)
where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

data Grid
  = Grid {
    gridId :: Int,
    cells :: [Cell],
    solved :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON Grid
instance FromJSON Grid

data Cell
  = Cell {
    cellId :: Int,
    row :: Int,
    col :: Int,
    realValue :: Int,
    userValue :: Int,
    revealed :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON Cell
instance FromJSON Cell