{-# LANGUAGE DeriveGeneric #-}

module Domain
( Grid(..)
, Cell(..)
)
where

import qualified GHC.Generics as Generics (Generic)
import qualified Data.Aeson as Aeson (FromJSON, ToJSON)

data Grid
  = Grid {
    gridId :: Integer,
    cells :: [[Cell]],
    isSolved :: Bool
  }
  deriving (Eq, Show, Generics.Generic)

instance Aeson.ToJSON Grid
instance Aeson.FromJSON Grid

data Cell
  = Cell {
    cellId :: Integer,
    row :: Int,
    col :: Int,
    realValue :: Int,
    userValue :: Int,
    isRevealed :: Bool
  }
  deriving (Eq, Show, Generics.Generic)

instance Aeson.ToJSON Cell
instance Aeson.FromJSON Cell