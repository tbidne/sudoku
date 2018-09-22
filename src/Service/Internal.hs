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
)
where

import           Prelude    hiding   (id)
import           Data.Maybe          (fromMaybe)
import qualified Domain              (Cell(..), Grid(..))
import qualified Database as DB_Cell (CellT(..))
import qualified Database as DB_Grid (GridT(..))

blankGrid :: Domain.Grid
blankGrid = Domain.Grid 0 cells False
  where cells = replicate 81 exampleCell

exampleCell :: Domain.Cell
exampleCell = Domain.Cell 0 0 0 0 0 False

-- transformers

gridTToGrid :: [DB_Grid.GridT] -> Maybe [Domain.Cell] -> Maybe Domain.Grid
gridTToGrid _ Nothing = Nothing
gridTToGrid [] _ = Nothing
gridTToGrid [gridT] mCells =
  case mCells of
    Nothing -> Nothing
    Just cells -> Just $ Domain.Grid id cells solved
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

-- remember to make set flag true here
solve :: Domain.Grid -> (Domain.Grid, Bool)
solve grid =
  let cells = Domain.cells grid in
  case getEmptyCell cells of
    Nothing -> (grid, validate grid)
    Just empty ->
      let allGuesses = allGuessesForCell grid empty in
      let filtered = filter validate allGuesses in
      let possibleSolns = map solve filtered in
      fromMaybe (grid, False) (getSolvedGrid possibleSolns)

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

getSolvedGrid :: [(Domain.Grid, Bool)] -> Maybe (Domain.Grid, Bool)
getSolvedGrid [] = Nothing
getSolvedGrid pairs = takeFirst snd pairs

getEmptyCell :: [Domain.Cell] -> Maybe Domain.Cell
getEmptyCell [] = Nothing
getEmptyCell cells = takeFirst (\c -> Domain.realValue c == -1) cells

-- TODO: maybe swap with take 1 $ ...
takeFirst :: (a -> Bool) -> [a] -> Maybe a
takeFirst _ [] = Nothing
takeFirst f (a:as)
  | f a       = Just a
  | otherwise = takeFirst f as