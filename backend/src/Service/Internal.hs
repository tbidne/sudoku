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
, markCellsRevealed
, revealAllCells
)
where

import           Prelude      hiding (id)
import           Data.Maybe          (fromMaybe)
import qualified Domain              (Cell(..), Grid(..))
import qualified Database as DB_Cell (CellT(..))
import qualified Database as DB_Grid (GridT(..))

blankGrid :: Domain.Grid
blankGrid = Domain.Grid 0 cells False
  where initCells = zipWith (\id (row, col) -> Domain.Cell id row col 0 0 False)
        cells = initCells [0..] [(x, y) | x <- [1..9], y <- [1..9]]

-- transformers

gridTToGrid :: [DB_Grid.GridT] -> Maybe [Domain.Cell] -> Maybe Domain.Grid
gridTToGrid _ Nothing = Nothing
gridTToGrid [] _ = Nothing
gridTToGrid [gridT] (Just cells) = Just $ Domain.Grid id cells solved
    where id = DB_Grid.gridId gridT
          solved = DB_Grid.solved gridT
gridTToGrid _ _ = Nothing

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

-- TODO: make this (boxes) better
validate :: Domain.Grid -> Bool
validate grid = rowsValid && colsValid && boxesValid
  where cells = Domain.cells grid
        fieldFnList f = replicate 9 (\x -> filter (\c -> f c == x) cells)
        rows = zipWith (\f n -> f n) (fieldFnList Domain.row) [0..8]
        cols = zipWith (\f n -> f n) (fieldFnList Domain.col) [0..8]

        boxNW = filter (\c -> Domain.row c <= 2 && Domain.col c <= 2) cells
        boxNC = filter (\c -> Domain.row c >= 3 && Domain.row c <= 5 && Domain.col c <= 2) cells
        boxNE = filter (\c -> Domain.row c >= 6 && Domain.col c < 3) cells

        boxCW = filter (\c -> Domain.row c <= 2 && Domain.col c >= 3 && Domain.col c <= 5) cells
        boxCC = filter (\c -> Domain.row c >= 3 && Domain.row c <= 5 && Domain.col c >= 3 && Domain.col c <= 5) cells
        boxCE = filter (\c -> Domain.row c >= 6 && Domain.col c >= 3 && Domain.col c <= 5) cells

        boxSW = filter (\c -> Domain.row c <= 2 && Domain.col c >= 6) cells
        boxSC = filter (\c -> Domain.row c >= 3 && Domain.row c <= 5 && Domain.col c >= 6) cells
        boxSE = filter (\c -> Domain.row c >= 6 && Domain.col c >= 6) cells

        boxes = [boxNW, boxNC, boxNE, boxCW, boxCC, boxCE, boxSW, boxSC, boxSE]

        rowsValid = foldr ((&&) . validateSection) True rows
        colsValid = foldr ((&&) . validateSection) True cols
        boxesValid = foldr ((&&) . validateSection) True boxes

-- this works by scanning a given section and making sure each 1 <= x <= 9 is in the list
-- uniqueValList is a list of lists, where each element is a list of all cells with that realValue
-- i.e., uniqueValList[0] has all cells with realValue 1`, uniqueValList[1] has all cells with realValue 2, ...
-- if a number is NOT in the list then that entry in uniqueValList will instead by the empty list
-- this means we can test for validity by asserting that each element has length < 2, i.e., no element in [1..9]
-- shows up more than once
validateSection :: [Domain.Cell] -> Bool
validateSection cells = foldr ((&&) . (\ x -> length x < 2)) True uniqueValList
    where fnList = replicate 9 (\x -> filter (\c -> Domain.realValue c == x) cells)
          uniqueValList = zipWith (\f n -> f n) fnList [1..9]


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

getSolvedGrid :: [Maybe Domain.Grid] -> Maybe Domain.Grid
getSolvedGrid [] = Nothing
getSolvedGrid (g:gs) =
  case g of
    Nothing -> getSolvedGrid gs
    Just grid -> Just grid

getEmptyCell :: [Domain.Cell] -> Maybe Domain.Cell
getEmptyCell [] = Nothing
getEmptyCell cells = takeFirst (\c -> Domain.realValue c `elem` [-1, 0]) cells

-- TODO: maybe swap with take 1 $ ...
takeFirst :: (a -> Bool) -> [a] -> Maybe a
takeFirst _ [] = Nothing
takeFirst f (a:as)
  | f a       = Just a
  | otherwise = takeFirst f as

-- mark grid functions

revealGrid :: Domain.Grid -> Domain.Grid
revealGrid grid = Domain.Grid id cells True
  where id = Domain.gridId grid
        cells = revealAllCells $ Domain.cells grid

revealCell :: Domain.Cell -> Domain.Cell
revealCell cell = Domain.Cell id row col realValue userValue True
  where id = Domain.cellId cell
        row = Domain.row cell
        col = Domain.col cell
        realValue = Domain.realValue cell
        userValue = Domain.userValue cell

markGridSolved :: Domain.Grid -> Domain.Grid
markGridSolved grid = Domain.Grid id cells True
  where id = Domain.gridId grid
        cells = markCellsRevealed $ Domain.cells grid

markCellsRevealed :: [Domain.Cell] -> [Domain.Cell]
markCellsRevealed = markCells f
  where f c = Domain.userValue c == Domain.realValue c && Domain.realValue c /= 0

revealAllCells :: [Domain.Cell] -> [Domain.Cell]
revealAllCells = markCells f
  where f _ = True

markCells :: (Domain.Cell -> Bool) -> [Domain.Cell] -> [Domain.Cell]
markCells f = map newCell
  where newCell c = Domain.Cell (Domain.cellId c) (Domain.row c)
          (Domain.col c) (Domain.realValue c) (Domain.userValue c)
          (f c)