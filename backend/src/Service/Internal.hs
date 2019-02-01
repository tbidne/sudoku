{-|
Module      : API
Description : Sudoku Internal functions
License     : MIT
Maintainer  : tbidne@gmail.com

Contains internal functions for Sudoku.
-}
module Service.Internal
( blankGrid
, gridTToGrid
, cellTToCell
, solve
, allGuessesForCell
, getEmptyCell
, getSolvedGrid
, validate
, validateSection
, revealCell
, revealGrid
, markGridSolved
, markCorrectCellsRevealed
, revealAllCells
)
where

import           Prelude      hiding (id)
import           Data.Maybe          (fromMaybe)
import qualified Domain              (Cell(..), Grid(..))
import qualified Database as DB_Cell (CellT(..))
import qualified Database as DB_Grid (GridT(..))

-- | Returns a blank 'Domain.Grid'
blankGrid :: Domain.Grid
blankGrid = Domain.Grid 0 cells False
  where initCells = zipWith (\id (row, col) -> Domain.Cell id row col 0 0 False)
        cells = initCells [0..] [(x, y) | x <- [0..8], y <- [0..8]]

-- | For a ['DB_Grid.GridT'] of size 1, and 'Just' ['Domain.Cell'], returns
-- 'Just' 'Domain.Grid'. Otherwise returns 'Nothing'.
gridTToGrid :: [DB_Grid.GridT] -> Maybe [Domain.Cell] -> Maybe Domain.Grid
gridTToGrid _ Nothing = Nothing
gridTToGrid [] _ = Nothing
gridTToGrid [gridT] (Just cells) = Just $ Domain.Grid id cells solved
    where id = DB_Grid.gridId gridT
          solved = DB_Grid.solved gridT
gridTToGrid _ _ = Nothing

-- | For a ['DB_Cell.CellT'], returns 'Just' ['Domain.Cell'].
-- Otherwise returns 'Nothing'.
cellTToCell :: [DB_Cell.CellT] -> Maybe [Domain.Cell]
cellTToCell [] = Nothing
cellTToCell cellTs = Just $ map (\cellT ->
    let id = DB_Cell.cellId cellT in
    let row = DB_Cell.row cellT in
    let col = DB_Cell.col cellT in
    let realValue = fromMaybe (-1) $ DB_Cell.realValue cellT in
    let userValue = fromMaybe (-1) $ DB_Cell.userValue cellT in
    let revealed = DB_Cell.revealed cellT in
    Domain.Cell id row col realValue userValue revealed
  ) cellTs

-- main solving

-- | If 'Domain.Grid' @g@ can be solved returns 'Just' @g'@,
-- where @g'@ is solved. Otherwise returns 'Nothing'.
solve :: Domain.Grid -> Maybe Domain.Grid
solve grid =
  let cells = Domain.cells grid in
  case getEmptyCell cells of
    Nothing -> if validate grid then Just $ markGridSolved grid else Nothing
    Just empty ->
      let allGuesses = allGuessesForCell grid empty in
      let filtered = filter validate allGuesses in
      let possibleSolns = map solve filtered in
      getSolvedGrid possibleSolns

-- | Returns 'True' if the 'Domain.Grid' is valid by the rules of Sudoku,
-- 'False' otherwise.
validate :: Domain.Grid -> Bool
validate grid = rowsValid && colsValid && boxesValid
  where cells = Domain.cells grid
        rows = groupCellsByFn cells [0..8] Domain.row
        cols = groupCellsByFn cells [0..8] Domain.col

        boxNW = getBox cells 0 2 0 2
        boxNC = getBox cells 0 2 3 5
        boxNE = getBox cells 0 2 6 8
        boxCW = getBox cells 3 5 0 2
        boxCC = getBox cells 3 5 3 5
        boxCE = getBox cells 3 5 6 8
        boxSW = getBox cells 6 8 0 2
        boxSC = getBox cells 6 8 3 5
        boxSE = getBox cells 6 8 6 8
        boxes = [boxNW, boxNC, boxNE, boxCW, boxCC, boxCE, boxSW, boxSC, boxSE]

        rowsValid = foldr ((&&) . validateSection) True rows
        colsValid = foldr ((&&) . validateSection) True cols
        boxesValid = foldr ((&&) . validateSection) True boxes

-- | For a 'Domain.Cell' 'section' (i.e., row, column, or box), returns 'True' if the section
-- is valid, 'False' otherwise.
--
-- Works by filtering the ['Domain.Cell'] into [['Domain.Cell']] where each list contains
-- the list of all cells with @realValue@ \(r \in {1,2,\ldots 9}\). After that we test that each
-- list has length @< 2@.
validateSection :: [Domain.Cell] -> Bool
validateSection cells = foldr ((&&) . (\xs -> length xs < 2)) True uniqueValList
    where uniqueValList = groupCellsByFn cells [1..9] Domain.realValue

-- | For ['Domain.Cell'] @cells@, ['Int'] @groups@, and cell function @f@, groups
-- the cells by @f@ into @groups@.
groupCellsByFn :: [Domain.Cell] -> [Int] -> (Domain.Cell -> Int) -> [[Domain.Cell]]
groupCellsByFn cells groups f = [g y | y <- groups]
  where g x = filter (\c -> f c == x) cells

-- | Returns a Sudoku 'box' based on the range params, e.g., North West
getBox :: [Domain.Cell] -> Int -> Int -> Int -> Int -> [Domain.Cell]
getBox cells rLow rHigh cLow cHigh = filter f cells
  where dr = Domain.row
        dc = Domain.col
        f c = dr c >= rLow && dr c <= rHigh && dc c >= cLow && dc c <= cHigh

-- | For a @grid@ and @cell@, returns a list of grids where each new
-- grid is all possible guesses for @cell@.
allGuessesForCell :: Domain.Grid -> Domain.Cell -> [Domain.Grid]
allGuessesForCell grid cell = guesses
  where cells = Domain.cells grid
        temp = filter (\c -> Domain.cellId c /= Domain.cellId cell) cells
        guesses = guessHelper grid temp cell [1..9] []

guessHelper :: Domain.Grid -> [Domain.Cell] -> Domain.Cell -> [Int] -> [Domain.Grid] -> [Domain.Grid]
guessHelper _ _ _ [] acc = acc
guessHelper grid cells cell (x:xs) acc = guessHelper grid cells cell xs $ acc ++ [grid']
  where cell' = Domain.Cell
                  (Domain.cellId cell)
                  (Domain.row cell)
                  (Domain.col cell)
                  x
                  (Domain.userValue cell)
                  (Domain.revealed cell)
        grid' = Domain.Grid
                  (Domain.gridId grid)
                  (cells ++ [cell'])
                  False

-- | Returns a solved grid, if any exists.
getSolvedGrid :: [Maybe Domain.Grid] -> Maybe Domain.Grid
getSolvedGrid = foldr f Nothing
  where f (Just g) _ = Just g
        f _ (Just g) = Just g
        f _ _ = Nothing

-- | Returns an empty cell, if any exists.
getEmptyCell :: [Domain.Cell] -> Maybe Domain.Cell
getEmptyCell cells = foldr f Nothing cells
  where empty x = Domain.realValue x `elem` [-1,0]
        f _ (Just c) = Just c
        f c _
          | empty c   = Just c
          | otherwise = Nothing

-- mark grid functions

-- | Marks a grid as revealed.
revealGrid :: Domain.Grid -> Domain.Grid
revealGrid grid = Domain.Grid id cells True
  where id = Domain.gridId grid
        cells = revealAllCells $ Domain.cells grid

-- | Marks a cell as revealed.
revealCell :: Domain.Cell -> Domain.Cell
revealCell cell = Domain.Cell id row col realValue userValue True
  where id = Domain.cellId cell
        row = Domain.row cell
        col = Domain.col cell
        realValue = Domain.realValue cell
        userValue = Domain.userValue cell

-- | Marks a grid as solved.
markGridSolved :: Domain.Grid -> Domain.Grid
markGridSolved grid = Domain.Grid id cells True
  where id = Domain.gridId grid
        cells = markCorrectCellsRevealed $ Domain.cells grid

-- | Marks correct cells as revealed.
markCorrectCellsRevealed :: [Domain.Cell] -> [Domain.Cell]
markCorrectCellsRevealed = markCells f
  where f c = Domain.userValue c == Domain.realValue c && Domain.realValue c /= 0

-- | Marks all cells as revealed.
revealAllCells :: [Domain.Cell] -> [Domain.Cell]
revealAllCells = markCells f
  where f _ = True

markCells :: (Domain.Cell -> Bool) -> [Domain.Cell] -> [Domain.Cell]
markCells f = map newCell
  where newCell c = Domain.Cell (Domain.cellId c) (Domain.row c)
          (Domain.col c) (Domain.realValue c) (Domain.userValue c)
          (f c)