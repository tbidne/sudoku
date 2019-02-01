{-# LANGUAGE DeriveGeneric #-}

{-|
Module      : Domain
Description : Sudoku Domain Types
License     : MIT
Maintainer  : tbidne@gmail.com

Contains the data declarations for Sudoku Grid and Cell
-}
module Domain
( Grid(..)
, Cell(..)
)
where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

-- | Represents a Sudoku Grid.
--
-- 'Int' @gridId@: id for the grid.
--
-- ['Cell'] @cells@: list of all cells for the grid.
--
-- 'Bool' @solved@: indicates if this grid has been solved.
data Grid
  = Grid {
    gridId :: Int,
    cells :: [Cell],
    solved :: Bool
  }
  deriving (Eq, Show, Generic)

instance ToJSON Grid
instance FromJSON Grid

-- | Represents a Sudoku Cell.
--
-- 'Int' @cellId@: cell's id.
--
-- 'Int' @row@: cell's row.
--
-- 'Int' @col@: cell's column.
--
-- 'Int' @realValue@: cell's real value if it's been solved, otherwise -1 (DB) or 0 (UI).
--
-- 'Int' @userValue@: cell's user provided value, otherwise -1.
--
-- 'Bool' @revealed@: indicates if this grid has been revealed.
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