module Service.InternalSpec
( spec
)
where

import           Test.Hspec          (describe, it, Spec, shouldBe, shouldSatisfy, shouldContain)
import           Data.Maybe          (fromJust, isJust)
import qualified Domain              (Cell(..), Grid(..))
import qualified Database as DB_Cell (CellT(..))
import qualified Database as DB_Grid (GridT(..))
import qualified Service.MockValidationGrids as MVG
import qualified Service.MockSolveGrids as MSG
import           Service.Internal

spec :: Spec
spec = do
  describe "Grid transformations" $ do
    it "gridTToGrid for Nothing cells should return Nothing" $ do
      let gridT = mockGridT
      gridTToGrid [gridT] Nothing `shouldBe` Nothing

    it "gridTToGrid for empty grid should return Nothing" $ do
      let jCells = Just mockCells
      gridTToGrid [] jCells `shouldBe` Nothing

    it "gridTToGrid for non-empty grid and Just cells should return Grid" $ do
      let cells = mockCells
      let gridT = mockGridT

      let jGrid = gridTToGrid [gridT] $ Just cells

      jGrid `shouldSatisfy` isJust
      let result = fromJust jGrid

      Domain.gridId result `shouldBe` DB_Grid.gridId gridT
      Domain.solved result `shouldBe` DB_Grid.solved gridT
      Domain.cells result `shouldBe` cells

  describe "Cell transformations" $ do
    it "cellTToCell for empty cells should return Nothing" $ do
      cellTToCell [] `shouldBe` Nothing

    it "cellTToCell for non-empty cells should return Cells" $ do
      let c1 = Domain.Cell 0 0 1 5 3 True
      let c2 = Domain.Cell 1 1 1 (-1) (-1) False
      let cellTs = mockCellTs

      let jCells = cellTToCell cellTs
      let results = fromJust jCells

      length results `shouldBe` 2
      results `shouldBe` [c1, c2]

  describe "allGuessesForCell tests" $ do
    it "allGuessesForCell should return a list of new grids with all guesses for cell" $ do
      let grid = mockGridWithCells
      let cell = Domain.cells grid !! 1

      let results = allGuessesForCell grid cell

      length results `shouldBe` 9
      results `shouldContain` [copyGridWithGuessedCell grid 1]
      results `shouldContain` [copyGridWithGuessedCell grid 2]
      results `shouldContain` [copyGridWithGuessedCell grid 3]
      results `shouldContain` [copyGridWithGuessedCell grid 4]
      results `shouldContain` [copyGridWithGuessedCell grid 5]
      results `shouldContain` [copyGridWithGuessedCell grid 6]
      results `shouldContain` [copyGridWithGuessedCell grid 7]
      results `shouldContain` [copyGridWithGuessedCell grid 8]
      results `shouldContain` [copyGridWithGuessedCell grid 9]

  describe "getSolvedGrid tests" $ do
    it "getSolvedGrid for empty list should return Nothing" $ do
      getSolvedGrid [] `shouldBe` Nothing

    it "getSolvedGrid for list with no solved grids return Nothing" $ do
      let grids = mockGridMaybes
      let results = getSolvedGrid grids

      results `shouldBe` Nothing

    it "getSolvedGrid for list with solved grid should return Just grid" $ do
      let grids = mockGridMaybesWithSolved
      let result = getSolvedGrid grids

      result `shouldBe` grids !! 1

  describe "getEmptyCell tests" $ do
    it "getEmptyCell for empty list should return Nothing" $ do
      getEmptyCell [] `shouldBe` Nothing

    it "getEmptyCell for list with no empty cells should return Nothing" $ do
      let cells = mockCells
      let results = getEmptyCell cells

      results `shouldBe` Nothing

    it "getEmptyCell for list with empty cell should return Cell" $ do
      let cells = mockCellsWithEmpty
      let jCell = getEmptyCell cells

      jCell `shouldSatisfy` isJust
      let result = fromJust jCell

      result `shouldBe` cells !! 1

  describe "validate tests" $ do
    it "should validate correct grid 1" $ do
      validate MVG.correctGrid1 `shouldBe` True

    it "should validate empty grid" $ do
      validate MVG.emptyGrid `shouldBe` True

    it "should invalidate almost correct grid" $ do
      validate MVG.almostCorrectGrid `shouldBe` False

    it "should invalidate grid with row error" $ do
      validate MVG.gridRowError `shouldBe` False

    it "should invalidate grid with col error" $ do
      validate MVG.gridColError `shouldBe` False

    it "should invalidate grid with box error" $ do
      validate MVG.gridBoxError `shouldBe` False

    it "should invalidate grid with box errors" $ do
      validate MVG.gridFullBoxError `shouldBe` False

  describe "solve tests" $ do
    it "should return solved grid" $ do
      let result = solve MSG.gridOneSolved

      result `shouldSatisfy` isJust
      fromJust result `shouldSatisfy` gridEquals MSG.gridOneSolved

    it "should solve trivial grid" $ do
      let result = solve MSG.gridOneTrivial

      result `shouldSatisfy` isJust
      fromJust result `shouldSatisfy` gridEquals MSG.gridOneSolved

    it "should solve easy grid" $ do
      let result = solve MSG.gridOneEasy

      result `shouldSatisfy` isJust
      fromJust result `shouldSatisfy` gridEquals MSG.gridOneSolved

    it "should solve medium grid" $ do
      let result = solve MSG.gridOneMedium

      result `shouldSatisfy` isJust
      fromJust result `shouldSatisfy` gridEquals MSG.gridOneSolved

    it "should not solve impossible grid" $ do
      let result = solve MSG.gridOneImpossible
      result `shouldBe` Nothing

    it "should solve real grid one" $ do
      let result = solve MSG.gridOneReal

      result `shouldSatisfy` isJust
      fromJust result `shouldSatisfy` gridEquals MSG.gridOneSolved

    -- this is too hard for travis apparently, so commenting out for now
    --it "should solve real grid two" $ do
    --  let (grid', result) = solve MSG.gridTwoReal

    --  result `shouldBe` True
    --  grid' `shouldSatisfy` gridEquals MSG.gridTwoSolved

  describe "mark tests" $ do
    it "should reveal cells where user and real values nonzero and equal" $ do
      let result = markCellsRevealed mockCells
      map Domain.revealed result `shouldBe` [False, True, False]

    it "should reveal all where user and real values nonzero and equal" $ do
      let result = revealAllCells mockCells
      map Domain.revealed result `shouldBe` [True, True, True]

mockGridT :: DB_Grid.GridT
mockGridT = DB_Grid.GridT 1 True

mockCellTs :: [DB_Cell.CellT]
mockCellTs = [c1, c2]
  where c1 = DB_Cell.CellT 0 0 0 1 (Just 5) (Just 3) True
        c2 = DB_Cell.CellT 1 0 1 1 Nothing Nothing False

mockCells :: [Domain.Cell]
mockCells = [c1, c2, c3]
  where c1 = Domain.Cell 0 0 0 5 3 False
        c2 = Domain.Cell 1 1 1 2 2 False
        c3 = Domain.Cell 1 1 1 0 0 False

mockCellsWithEmpty :: [Domain.Cell]
mockCellsWithEmpty = [c1, c2, c3]
  where c1 = Domain.Cell 0 0 0 5 3 True
        c2 = Domain.Cell 1 1 1 (-1) 2 False
        c3 = Domain.Cell 2 1 3 2 2 False

copyGridWithGuessedCell :: Domain.Grid -> Int -> Domain.Grid
copyGridWithGuessedCell grid i = Domain.Grid (Domain.gridId grid) [c1, c3, c2'] (Domain.solved grid)
  where [c1, c2, c3] = mockCellsWithEmpty
        c2' = Domain.Cell
                (Domain.cellId c2)
                (Domain.row c2)
                (Domain.col c2)
                i
                (Domain.userValue c2)
                (Domain.revealed c2)

mockGridWithCells :: Domain.Grid
mockGridWithCells = Domain.Grid 0 cells False
  where cells = mockCellsWithEmpty

mockGrids :: [Domain.Grid]
mockGrids = [g1, g2, g3]
  where g1 = Domain.Grid 0 [] False
        g2 = Domain.Grid 1 [] False
        g3 = Domain.Grid 2 [] False

mockGridsWithSolved :: [Domain.Grid]
mockGridsWithSolved = [g1, g2, g3]
  where g1 = Domain.Grid 0 [] False
        g2 = Domain.Grid 1 [] True
        g3 = Domain.Grid 2 [] False

mockGridMaybes :: [Maybe Domain.Grid]
mockGridMaybes = map (\g -> if Domain.solved g then Just g else Nothing) mockGrids

mockGridMaybesWithSolved :: [Maybe Domain.Grid]
mockGridMaybesWithSolved = map (\g -> if Domain.solved g then Just g else Nothing) mockGridsWithSolved

findByIdAndCompareVal :: [Domain.Cell] -> Domain.Cell -> Bool
findByIdAndCompareVal cells c =
  let c' = head (filter (\x -> Domain.cellId x == Domain.cellId c) cells) in
  Domain.realValue c == Domain.realValue c'

gridEquals :: Domain.Grid -> Domain.Grid -> Bool
gridEquals one two = foldr ((&&) . findByIdAndCompareVal cellsOne) True cellsTwo
  where cellsOne = Domain.cells one
        cellsTwo = Domain.cells two