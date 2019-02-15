module Service.InternalSpec
( spec
)
where

import           Test.Hspec          (before, describe, it, Spec, SpecWith, shouldBe, shouldSatisfy)
import           Data.Maybe          (fromJust, isJust)
import           Data.Text.Lazy      (pack, toLower)
import qualified Domain              (Cell(..), Grid(..))
import qualified Database as DB_Cell (CellT(..))
import qualified Database as DB_Grid (GridT(..))
import qualified System.Environment as Sys (lookupEnv)
import qualified Service.MockValidationGrids as MVG
import qualified Service.MockSolveGrids as MSG
import           Service.Internal

getTravisEnv :: IO Bool
getTravisEnv = Sys.lookupEnv "TRAVIS" >>= \case
  Just s  -> return $ "true" == convert s
  Nothing -> return False
  where convert = toLower . pack

spec :: Spec
spec = before getTravisEnv specWithTravisEnv

specWithTravisEnv :: SpecWith Bool
specWithTravisEnv = do
  describe "Grid transformations" $ do
    it "gridTToGrid for Nothing cells should return Nothing" $ \_ -> do
      let gridT = mockGridT
      gridTToGrid [gridT] Nothing `shouldBe` Nothing

    it "gridTToGrid for empty grid should return Nothing" $ \_ -> do
      let jCells = Just mockCells
      gridTToGrid [] jCells `shouldBe` Nothing

    it "gridTToGrid for non-empty grid and Just cells should return Grid" $ \_ -> do
      let cells = mockCells
      let gridT = mockGridT

      let jGrid = gridTToGrid [gridT] $ Just cells

      jGrid `shouldSatisfy` isJust
      let result = fromJust jGrid

      Domain.gridId result `shouldBe` DB_Grid.gridId gridT
      Domain.solved result `shouldBe` DB_Grid.solved gridT
      Domain.cells result `shouldBe` cells

  describe "Cell transformations" $ do
    it "cellTToCell for empty cells should return Nothing" $ \_ -> do
      cellTToCell [] `shouldBe` Nothing

    it "cellTToCell for non-empty cells should return Cells" $ \_ -> do
      let c1 = Domain.Cell 0 0 1 5 3 True
      let c2 = Domain.Cell 1 1 1 (-1) (-1) False
      let cellTs = mockCellTs

      let jCells = cellTToCell cellTs
      let results = fromJust jCells

      length results `shouldBe` 2
      results `shouldBe` [c1, c2]

  describe "allGuessesForCell tests" $ do
    it "allGuessesForCell should return a list of new grids with all guesses for cell" $ \_ -> do
      let grid = mockGridWithCells
      let cell = Domain.cells grid !! 1

      let results = allGuessesForCell grid cell

      length results `shouldBe` 9
      gridListContains results (copyGridWithGuessedCell grid 1) `shouldBe` True
      gridListContains results (copyGridWithGuessedCell grid 2) `shouldBe` True
      gridListContains results (copyGridWithGuessedCell grid 3) `shouldBe` True
      gridListContains results (copyGridWithGuessedCell grid 4) `shouldBe` True
      gridListContains results (copyGridWithGuessedCell grid 5) `shouldBe` True
      gridListContains results (copyGridWithGuessedCell grid 6) `shouldBe` True
      gridListContains results (copyGridWithGuessedCell grid 7) `shouldBe` True
      gridListContains results (copyGridWithGuessedCell grid 8) `shouldBe` True
      gridListContains results (copyGridWithGuessedCell grid 9) `shouldBe` True

  describe "getSolvedGrid tests" $ do
    it "getSolvedGrid for empty list should return Nothing" $ \_ -> do
      getSolvedGrid [] `shouldBe` Nothing

    it "getSolvedGrid for list with no solved grids return Nothing" $ \_ -> do
      let grids = mockGridMaybes
      let results = getSolvedGrid grids

      results `shouldBe` Nothing

    it "getSolvedGrid for list with solved grid should return Just grid" $ \_ -> do
      let grids = mockGridMaybesWithSolved
      let result = getSolvedGrid grids

      result `shouldBe` grids !! 1

  describe "getEmptyCell tests" $ do
    it "getEmptyCell for empty list should return Nothing" $ \_ -> do
      getEmptyCell [] `shouldBe` Nothing

    it "getEmptyCell for list with no empty cells should return Nothing" $ \_ -> do
      let cells = mockCellsWithoutEmpty
      let results = getEmptyCell cells

      results `shouldBe` Nothing

    it "getEmptyCell for list with empty cell should return Cell" $ \_ -> do
      let cells = mockCellsWithEmpty
      let jCell = getEmptyCell cells

      jCell `shouldSatisfy` isJust
      let result = fromJust jCell

      result `shouldBe` cells !! 1

  describe "validate tests" $ do
    it "should validate correct grid 1" $ \_ -> do
      validate MVG.correctGrid1 `shouldBe` True

    it "should validate empty grid" $ \_ -> do
      validate MVG.emptyGrid `shouldBe` True

    it "should invalidate almost correct grid" $ \_ -> do
      validate MVG.almostCorrectGrid `shouldBe` False

    it "should invalidate grid with row error" $ \_ -> do
      validate MVG.gridRowError `shouldBe` False

    it "should invalidate grid with col error" $ \_ -> do
      validate MVG.gridColError `shouldBe` False

    it "should invalidate grid with box error" $ \_ -> do
      validate MVG.gridBoxError `shouldBe` False

    it "should invalidate grid with box errors" $ \_ -> do
      validate MVG.gridFullBoxError `shouldBe` False

  describe "solve tests" $ do
    it "should return solved grid" $ \_ -> do
      let result = solve MSG.gridOneSolved

      result `shouldSatisfy` isJust
      fromJust result `shouldSatisfy` gridEquals MSG.gridOneSolved

    it "should solve trivial grid" $ \_ -> do
      let result = solve MSG.gridOneTrivial

      result `shouldSatisfy` isJust
      fromJust result `shouldSatisfy` gridEquals MSG.gridOneSolved

    it "should solve easy grid" $ \_ -> do
      let result = solve MSG.gridOneEasy

      result `shouldSatisfy` isJust
      fromJust result `shouldSatisfy` gridEquals MSG.gridOneSolved

    it "should solve medium grid" $ \_ -> do
      let result = solve MSG.gridOneMedium

      result `shouldSatisfy` isJust
      fromJust result `shouldSatisfy` gridEquals MSG.gridOneSolved

    it "should not solve impossible grid" $ \_ -> do
      let result = solve MSG.gridOneImpossible
      result `shouldBe` Nothing

    it "should solve real grid one" $ \_ -> do
      let result = solve MSG.gridOneReal

      result `shouldSatisfy` isJust
      fromJust result `shouldSatisfy` gridEquals MSG.gridOneSolved

    it "should solve real grid two" $ \_ -> do
          let result = solve MSG.gridTwoReal
          result `shouldSatisfy` isJust
          fromJust result `shouldSatisfy` gridEquals MSG.gridTwoSolved

  describe "mark tests" $ do
    it "should reveal cells where user and real values nonzero and equal" $ \_ -> do
      let result = markCorrectCellsRevealed mockCells
      map Domain.revealed result `shouldBe` [False, True, False]

    it "should reveal all where user and real values nonzero and equal" $ \_ -> do
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

mockCellsWithoutEmpty :: [Domain.Cell]
mockCellsWithoutEmpty = [c1, c2]
  where c1 = Domain.Cell 0 0 0 5 3 False
        c2 = Domain.Cell 1 1 1 2 2 False

mockCellsWithEmpty :: [Domain.Cell]
mockCellsWithEmpty = tripleToList mockCellsWithEmptyTuple

mockCellsWithEmptyTuple :: (Domain.Cell, Domain.Cell, Domain.Cell)
mockCellsWithEmptyTuple = (c1, c2, c3)
  where c1 = Domain.Cell 0 0 0 5 3 True
        c2 = Domain.Cell 1 1 1 (-1) 2 False
        c3 = Domain.Cell 2 1 3 2 2 False

copyGridWithGuessedCell :: Domain.Grid -> Int -> Domain.Grid
copyGridWithGuessedCell grid i = Domain.Grid (Domain.gridId grid) [c1, c3, c2'] (Domain.solved grid)
  where (c1, c2, c3) = mockCellsWithEmptyTuple
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

gridListContains :: [Domain.Grid] -> Domain.Grid -> Bool
gridListContains grids g = foldr ((||) . gridEquals g) False grids

tripleToList :: (a, a, a) -> [a]
tripleToList (a, b, c) = [a, b, c]