module Service.MockSolveGrids where

import qualified Domain (Cell(..), Grid(..))

gridOneSolved :: Domain.Grid
gridOneSolved = Domain.Grid 0
  [Domain.Cell 0 0 0 4 (-1) False,
  Domain.Cell 1 0 1 3 (-1) False,
  Domain.Cell 2 0 2 5 (-1) False,
  Domain.Cell 3 0 3 2 (-1) False,
  Domain.Cell 4 0 4 6 (-1) False,
  Domain.Cell 5 0 5 9 (-1) False,
  Domain.Cell 6 0 6 7 (-1) False,
  Domain.Cell 7 0 7 8 (-1) False,
  Domain.Cell 8 0 8 1 (-1) False,
  Domain.Cell 9 1 0 6 (-1) False,
  Domain.Cell 10 1 1 8 (-1) False,
  Domain.Cell 11 1 2 2 (-1) False,
  Domain.Cell 12 1 3 5 (-1) False,
  Domain.Cell 13 1 4 7 (-1) False,
  Domain.Cell 14 1 5 1 (-1) False,
  Domain.Cell 15 1 6 4 (-1) False,
  Domain.Cell 16 1 7 9 (-1) False,
  Domain.Cell 17 1 8 3 (-1) False,
  Domain.Cell 18 2 0 1 (-1) False,
  Domain.Cell 19 2 1 9 (-1) False,
  Domain.Cell 20 2 2 7 (-1) False,
  Domain.Cell 21 2 3 8 (-1) False,
  Domain.Cell 22 2 4 3 (-1) False,
  Domain.Cell 23 2 5 4 (-1) False,
  Domain.Cell 24 2 6 5 (-1) False,
  Domain.Cell 25 2 7 6 (-1) False,
  Domain.Cell 26 2 8 2 (-1) False,
  Domain.Cell 27 3 0 8 (-1) False,
  Domain.Cell 28 3 1 2 (-1) False,
  Domain.Cell 29 3 2 6 (-1) False,
  Domain.Cell 30 3 3 1 (-1) False,
  Domain.Cell 31 3 4 9 (-1) False,
  Domain.Cell 32 3 5 5 (-1) False,
  Domain.Cell 33 3 6 3 (-1) False,
  Domain.Cell 34 3 7 4 (-1) False,
  Domain.Cell 35 3 8 7 (-1) False,
  Domain.Cell 36 4 0 3 (-1) False,
  Domain.Cell 37 4 1 7 (-1) False,
  Domain.Cell 38 4 2 4 (-1) False,
  Domain.Cell 39 4 3 6 (-1) False,
  Domain.Cell 40 4 4 8 (-1) False,
  Domain.Cell 41 4 5 2 (-1) False,
  Domain.Cell 42 4 6 9 (-1) False,
  Domain.Cell 43 4 7 1 (-1) False,
  Domain.Cell 44 4 8 5 (-1) False,
  Domain.Cell 45 5 0 9 (-1) False,
  Domain.Cell 46 5 1 5 (-1) False,
  Domain.Cell 47 5 2 1 (-1) False,
  Domain.Cell 48 5 3 7 (-1) False,
  Domain.Cell 49 5 4 4 (-1) False,
  Domain.Cell 50 5 5 3 (-1) False,
  Domain.Cell 51 5 6 6 (-1) False,
  Domain.Cell 52 5 7 2 (-1) False,
  Domain.Cell 53 5 8 8 (-1) False,
  Domain.Cell 54 6 0 5 (-1) False,
  Domain.Cell 55 6 1 1 (-1) False,
  Domain.Cell 56 6 2 9 (-1) False,
  Domain.Cell 57 6 3 3 (-1) False,
  Domain.Cell 58 6 4 2 (-1) False,
  Domain.Cell 59 6 5 6 (-1) False,
  Domain.Cell 60 6 6 8 (-1) False,
  Domain.Cell 61 6 7 7 (-1) False,
  Domain.Cell 62 6 8 4 (-1) False,
  Domain.Cell 63 7 0 2 (-1) False,
  Domain.Cell 64 7 1 4 (-1) False,
  Domain.Cell 65 7 2 8 (-1) False,
  Domain.Cell 66 7 3 9 (-1) False,
  Domain.Cell 67 7 4 5 (-1) False,
  Domain.Cell 68 7 5 7 (-1) False,
  Domain.Cell 69 7 6 1 (-1) False,
  Domain.Cell 70 7 7 3 (-1) False,
  Domain.Cell 71 7 8 6 (-1) False,
  Domain.Cell 72 8 0 7 (-1) False,
  Domain.Cell 73 8 1 6 (-1) False,
  Domain.Cell 74 8 2 3 (-1) False,
  Domain.Cell 75 8 3 4 (-1) False,
  Domain.Cell 76 8 4 1 (-1) False,
  Domain.Cell 77 8 5 8 (-1) False,
  Domain.Cell 78 8 6 2 (-1) False,
  Domain.Cell 79 8 7 5 (-1) False,
  Domain.Cell 80 8 8 9 (-1) False]
  False

gridOneTrivial :: Domain.Grid
gridOneTrivial = Domain.Grid 0
  [Domain.Cell 0 0 0 4 (-1) False,
  Domain.Cell 1 0 1 3 (-1) False,
  Domain.Cell 2 0 2 5 (-1) False,
  Domain.Cell 3 0 3 2 (-1) False,
  Domain.Cell 4 0 4 6 (-1) False,
  Domain.Cell 5 0 5 9 (-1) False,
  Domain.Cell 6 0 6 7 (-1) False,
  Domain.Cell 7 0 7 8 (-1) False,
  Domain.Cell 8 0 8 1 (-1) False,
  Domain.Cell 9 1 0 6 (-1) False,
  Domain.Cell 10 1 1 8 (-1) False,
  Domain.Cell 11 1 2 2 (-1) False,
  Domain.Cell 12 1 3 5 (-1) False,
  Domain.Cell 13 1 4 7 (-1) False,
  Domain.Cell 14 1 5 1 (-1) False,
  Domain.Cell 15 1 6 4 (-1) False,
  Domain.Cell 16 1 7 9 (-1) False,
  Domain.Cell 17 1 8 3 (-1) False,
  Domain.Cell 18 2 0 1 (-1) False,
  Domain.Cell 19 2 1 9 (-1) False,
  Domain.Cell 20 2 2 7 (-1) False,
  Domain.Cell 21 2 3 8 (-1) False,
  Domain.Cell 22 2 4 3 (-1) False,
  Domain.Cell 23 2 5 4 (-1) False,
  Domain.Cell 24 2 6 5 (-1) False,
  Domain.Cell 25 2 7 6 (-1) False,
  Domain.Cell 26 2 8 2 (-1) False,
  Domain.Cell 27 3 0 8 (-1) False,
  Domain.Cell 28 3 1 2 (-1) False,
  Domain.Cell 29 3 2 6 (-1) False,
  Domain.Cell 30 3 3 1 (-1) False,
  Domain.Cell 31 3 4 9 (-1) False,
  Domain.Cell 32 3 5 5 (-1) False,
  Domain.Cell 33 3 6 3 (-1) False,
  Domain.Cell 34 3 7 4 (-1) False,
  Domain.Cell 35 3 8 7 (-1) False,
  Domain.Cell 36 4 0 3 (-1) False,
  Domain.Cell 37 4 1 7 (-1) False,
  Domain.Cell 38 4 2 4 (-1) False,
  Domain.Cell 39 4 3 6 (-1) False,
  Domain.Cell 40 4 4 8 (-1) False,
  Domain.Cell 41 4 5 2 (-1) False,
  Domain.Cell 42 4 6 9 (-1) False,
  Domain.Cell 43 4 7 1 (-1) False,
  Domain.Cell 44 4 8 5 (-1) False,
  Domain.Cell 45 5 0 9 (-1) False,
  Domain.Cell 46 5 1 5 (-1) False,
  Domain.Cell 47 5 2 1 (-1) False,
  Domain.Cell 48 5 3 7 (-1) False,
  Domain.Cell 49 5 4 4 (-1) False,
  Domain.Cell 50 5 5 3 (-1) False,
  Domain.Cell 51 5 6 6 (-1) False,
  Domain.Cell 52 5 7 2 (-1) False,
  Domain.Cell 53 5 8 8 (-1) False,
  Domain.Cell 54 6 0 5 (-1) False,
  Domain.Cell 55 6 1 1 (-1) False,
  Domain.Cell 56 6 2 9 (-1) False,
  Domain.Cell 57 6 3 3 (-1) False,
  Domain.Cell 58 6 4 2 (-1) False,
  Domain.Cell 59 6 5 6 (-1) False,
  Domain.Cell 60 6 6 8 (-1) False,
  Domain.Cell 61 6 7 7 (-1) False,
  Domain.Cell 62 6 8 4 (-1) False,
  Domain.Cell 63 7 0 2 (-1) False,
  Domain.Cell 64 7 1 4 (-1) False,
  Domain.Cell 65 7 2 8 (-1) False,
  Domain.Cell 66 7 3 9 (-1) False,
  Domain.Cell 67 7 4 5 (-1) False,
  Domain.Cell 68 7 5 7 (-1) False,
  Domain.Cell 69 7 6 1 (-1) False,
  Domain.Cell 70 7 7 3 (-1) False,
  Domain.Cell 71 7 8 6 (-1) False,
  Domain.Cell 72 8 0 7 (-1) False,
  Domain.Cell 73 8 1 6 (-1) False,
  Domain.Cell 74 8 2 3 (-1) False,
  Domain.Cell 75 8 3 4 (-1) False,
  Domain.Cell 76 8 4 1 (-1) False,
  Domain.Cell 77 8 5 8 (-1) False,
  Domain.Cell 78 8 6 2 (-1) False,
  Domain.Cell 79 8 7 5 (-1) False,
  Domain.Cell 80 8 8 (-1) (-1) False]
  False

gridOneEasy :: Domain.Grid
gridOneEasy = Domain.Grid 0
  [Domain.Cell 0 0 0 4 (-1) False,
  Domain.Cell 1 0 1 3 (-1) False,
  Domain.Cell 2 0 2 5 (-1) False,
  Domain.Cell 3 0 3 2 (-1) False,
  Domain.Cell 4 0 4 6 (-1) False,
  Domain.Cell 5 0 5 9 (-1) False,
  Domain.Cell 6 0 6 7 (-1) False,
  Domain.Cell 7 0 7 8 (-1) False,
  Domain.Cell 8 0 8 1 (-1) False,
  Domain.Cell 9 1 0 6 (-1) False,
  Domain.Cell 10 1 1 8 (-1) False,
  Domain.Cell 11 1 2 2 (-1) False,
  Domain.Cell 12 1 3 5 (-1) False,
  Domain.Cell 13 1 4 7 (-1) False,
  Domain.Cell 14 1 5 1 (-1) False,
  Domain.Cell 15 1 6 (-1) (-1) False,
  Domain.Cell 16 1 7 9 (-1) False,
  Domain.Cell 17 1 8 3 (-1) False,
  Domain.Cell 18 2 0 1 (-1) False,
  Domain.Cell 19 2 1 9 (-1) False,
  Domain.Cell 20 2 2 7 (-1) False,
  Domain.Cell 21 2 3 8 (-1) False,
  Domain.Cell 22 2 4 3 (-1) False,
  Domain.Cell 23 2 5 (-1) (-1) False,
  Domain.Cell 24 2 6 5 (-1) False,
  Domain.Cell 25 2 7 6 (-1) False,
  Domain.Cell 26 2 8 2 (-1) False,
  Domain.Cell 27 3 0 8 (-1) False,
  Domain.Cell 28 3 1 2 (-1) False,
  Domain.Cell 29 3 2 6 (-1) False,
  Domain.Cell 30 3 3 1 (-1) False,
  Domain.Cell 31 3 4 9 (-1) False,
  Domain.Cell 32 3 5 5 (-1) False,
  Domain.Cell 33 3 6 3 (-1) False,
  Domain.Cell 34 3 7 4 (-1) False,
  Domain.Cell 35 3 8 7 (-1) False,
  Domain.Cell 36 4 0 3 (-1) False,
  Domain.Cell 37 4 1 7 (-1) False,
  Domain.Cell 38 4 2 4 (-1) False,
  Domain.Cell 39 4 3 6 (-1) False,
  Domain.Cell 40 4 4 8 (-1) False,
  Domain.Cell 41 4 5 2 (-1) False,
  Domain.Cell 42 4 6 (-1) (-1) False,
  Domain.Cell 43 4 7 1 (-1) False,
  Domain.Cell 44 4 8 5 (-1) False,
  Domain.Cell 45 5 0 9 (-1) False,
  Domain.Cell 46 5 1 5 (-1) False,
  Domain.Cell 47 5 2 1 (-1) False,
  Domain.Cell 48 5 3 7 (-1) False,
  Domain.Cell 49 5 4 4 (-1) False,
  Domain.Cell 50 5 5 3 (-1) False,
  Domain.Cell 51 5 6 6 (-1) False,
  Domain.Cell 52 5 7 2 (-1) False,
  Domain.Cell 53 5 8 8 (-1) False,
  Domain.Cell 54 6 0 5 (-1) False,
  Domain.Cell 55 6 1 1 (-1) False,
  Domain.Cell 56 6 2 9 (-1) False,
  Domain.Cell 57 6 3 3 (-1) False,
  Domain.Cell 58 6 4 2 (-1) False,
  Domain.Cell 59 6 5 6 (-1) False,
  Domain.Cell 60 6 6 8 (-1) False,
  Domain.Cell 61 6 7 7 (-1) False,
  Domain.Cell 62 6 8 4 (-1) False,
  Domain.Cell 63 7 0 2 (-1) False,
  Domain.Cell 64 7 1 4 (-1) False,
  Domain.Cell 65 7 2 (-1) (-1) False,
  Domain.Cell 66 7 3 9 (-1) False,
  Domain.Cell 67 7 4 5 (-1) False,
  Domain.Cell 68 7 5 7 (-1) False,
  Domain.Cell 69 7 6 1 (-1) False,
  Domain.Cell 70 7 7 3 (-1) False,
  Domain.Cell 71 7 8 6 (-1) False,
  Domain.Cell 72 8 0 7 (-1) False,
  Domain.Cell 73 8 1 6 (-1) False,
  Domain.Cell 74 8 2 3 (-1) False,
  Domain.Cell 75 8 3 4 (-1) False,
  Domain.Cell 76 8 4 1 (-1) False,
  Domain.Cell 77 8 5 8 (-1) False,
  Domain.Cell 78 8 6 2 (-1) False,
  Domain.Cell 79 8 7 (-1) (-1) False,
  Domain.Cell 80 8 8 (-1) (-1) False]
  False

gridOneMedium :: Domain.Grid
gridOneMedium = Domain.Grid 0
  [Domain.Cell 0 0 0 4 (-1) False,
  Domain.Cell 1 0 1 3 (-1) False,
  Domain.Cell 2 0 2 5 (-1) False,
  Domain.Cell 3 0 3 (-1) (-1) False,
  Domain.Cell 4 0 4 6 (-1) False,
  Domain.Cell 5 0 5 9 (-1) False,
  Domain.Cell 6 0 6 7 (-1) False,
  Domain.Cell 7 0 7 8 (-1) False,
  Domain.Cell 8 0 8 (-1) (-1) False,
  Domain.Cell 9 1 0 6 (-1) False,
  Domain.Cell 10 1 1 8 (-1) False,
  Domain.Cell 11 1 2 2 (-1) False,
  Domain.Cell 12 1 3 5 (-1) False,
  Domain.Cell 13 1 4 7 (-1) False,
  Domain.Cell 14 1 5 1 (-1) False,
  Domain.Cell 15 1 6 (-1) (-1) False,
  Domain.Cell 16 1 7 9 (-1) False,
  Domain.Cell 17 1 8 3 (-1) False,
  Domain.Cell 18 2 0 1 (-1) False,
  Domain.Cell 19 2 1 9 (-1) False,
  Domain.Cell 20 2 2 7 (-1) False,
  Domain.Cell 21 2 3 8 (-1) False,
  Domain.Cell 22 2 4 3 (-1) False,
  Domain.Cell 23 2 5 (-1) (-1) False,
  Domain.Cell 24 2 6 5 (-1) False,
  Domain.Cell 25 2 7 6 (-1) False,
  Domain.Cell 26 2 8 2 (-1) False,
  Domain.Cell 27 3 0 8 (-1) False,
  Domain.Cell 28 3 1 (-1) (-1) False,
  Domain.Cell 29 3 2 6 (-1) False,
  Domain.Cell 30 3 3 1 (-1) False,
  Domain.Cell 31 3 4 9 (-1) False,
  Domain.Cell 32 3 5 5 (-1) False,
  Domain.Cell 33 3 6 3 (-1) False,
  Domain.Cell 34 3 7 4 (-1) False,
  Domain.Cell 35 3 8 7 (-1) False,
  Domain.Cell 36 4 0 3 (-1) False,
  Domain.Cell 37 4 1 (-1) (-1) False,
  Domain.Cell 38 4 2 4 (-1) False,
  Domain.Cell 39 4 3 6 (-1) False,
  Domain.Cell 40 4 4 8 (-1) False,
  Domain.Cell 41 4 5 2 (-1) False,
  Domain.Cell 42 4 6 (-1) (-1) False,
  Domain.Cell 43 4 7 1 (-1) False,
  Domain.Cell 44 4 8 5 (-1) False,
  Domain.Cell 45 5 0 9 (-1) False,
  Domain.Cell 46 5 1 5 (-1) False,
  Domain.Cell 47 5 2 (-1) (-1) False,
  Domain.Cell 48 5 3 7 (-1) False,
  Domain.Cell 49 5 4 4 (-1) False,
  Domain.Cell 50 5 5 3 (-1) False,
  Domain.Cell 51 5 6 6 (-1) False,
  Domain.Cell 52 5 7 2 (-1) False,
  Domain.Cell 53 5 8 (-1) (-1) False,
  Domain.Cell 54 6 0 5 (-1) False,
  Domain.Cell 55 6 1 1 (-1) False,
  Domain.Cell 56 6 2 9 (-1) False,
  Domain.Cell 57 6 3 3 (-1) False,
  Domain.Cell 58 6 4 2 (-1) False,
  Domain.Cell 59 6 5 (-1) (-1) False,
  Domain.Cell 60 6 6 8 (-1) False,
  Domain.Cell 61 6 7 7 (-1) False,
  Domain.Cell 62 6 8 4 (-1) False,
  Domain.Cell 63 7 0 2 (-1) False,
  Domain.Cell 64 7 1 4 (-1) False,
  Domain.Cell 65 7 2 (-1) (-1) False,
  Domain.Cell 66 7 3 9 (-1) False,
  Domain.Cell 67 7 4 5 (-1) False,
  Domain.Cell 68 7 5 7 (-1) False,
  Domain.Cell 69 7 6 1 (-1) False,
  Domain.Cell 70 7 7 3 (-1) False,
  Domain.Cell 71 7 8 6 (-1) False,
  Domain.Cell 72 8 0 (-1) (-1) False,
  Domain.Cell 73 8 1 6 (-1) False,
  Domain.Cell 74 8 2 3 (-1) False,
  Domain.Cell 75 8 3 (-1) (-1) False,
  Domain.Cell 76 8 4 1 (-1) False,
  Domain.Cell 77 8 5 8 (-1) False,
  Domain.Cell 78 8 6 2 (-1) False,
  Domain.Cell 79 8 7 (-1) (-1) False,
  Domain.Cell 80 8 8 (-1) (-1) False]
  False

gridOneImpossible :: Domain.Grid
gridOneImpossible = Domain.Grid 0
  [Domain.Cell 0 0 0 4 (-1) False,
  Domain.Cell 1 0 1 3 (-1) False,
  Domain.Cell 2 0 2 5 (-1) False,
  Domain.Cell 3 0 3 2 (-1) False,
  Domain.Cell 4 0 4 6 (-1) False,
  Domain.Cell 5 0 5 9 (-1) False,
  Domain.Cell 6 0 6 7 (-1) False,
  Domain.Cell 7 0 7 8 (-1) False,
  Domain.Cell 8 0 8 1 (-1) False,
  Domain.Cell 9 1 0 6 (-1) False,
  Domain.Cell 10 1 1 8 (-1) False,
  Domain.Cell 11 1 2 2 (-1) False,
  Domain.Cell 12 1 3 5 (-1) False,
  Domain.Cell 13 1 4 7 (-1) False,
  Domain.Cell 14 1 5 1 (-1) False,
  Domain.Cell 15 1 6 4 (-1) False,
  Domain.Cell 16 1 7 9 (-1) False,
  Domain.Cell 17 1 8 3 (-1) False,
  Domain.Cell 18 2 0 1 (-1) False,
  Domain.Cell 19 2 1 9 (-1) False,
  Domain.Cell 20 2 2 7 (-1) False,
  Domain.Cell 21 2 3 8 (-1) False,
  Domain.Cell 22 2 4 3 (-1) False,
  Domain.Cell 23 2 5 4 (-1) False,
  Domain.Cell 24 2 6 5 (-1) False,
  Domain.Cell 25 2 7 6 (-1) False,
  Domain.Cell 26 2 8 2 (-1) False,
  Domain.Cell 27 3 0 8 (-1) False,
  Domain.Cell 28 3 1 2 (-1) False,
  Domain.Cell 29 3 2 6 (-1) False,
  Domain.Cell 30 3 3 1 (-1) False,
  Domain.Cell 31 3 4 9 (-1) False,
  Domain.Cell 32 3 5 5 (-1) False,
  Domain.Cell 33 3 6 3 (-1) False,
  Domain.Cell 34 3 7 4 (-1) False,
  Domain.Cell 35 3 8 7 (-1) False,
  Domain.Cell 36 4 0 3 (-1) False,
  Domain.Cell 37 4 1 7 (-1) False,
  Domain.Cell 38 4 2 4 (-1) False,
  Domain.Cell 39 4 3 6 (-1) False,
  Domain.Cell 40 4 4 8 (-1) False,
  Domain.Cell 41 4 5 2 (-1) False,
  Domain.Cell 42 4 6 9 (-1) False,
  Domain.Cell 43 4 7 1 (-1) False,
  Domain.Cell 44 4 8 5 (-1) False,
  Domain.Cell 45 5 0 9 (-1) False,
  Domain.Cell 46 5 1 5 (-1) False,
  Domain.Cell 47 5 2 1 (-1) False,
  Domain.Cell 48 5 3 7 (-1) False,
  Domain.Cell 49 5 4 4 (-1) False,
  Domain.Cell 50 5 5 3 (-1) False,
  Domain.Cell 51 5 6 6 (-1) False,
  Domain.Cell 52 5 7 2 (-1) False,
  Domain.Cell 53 5 8 8 (-1) False,
  Domain.Cell 54 6 0 5 (-1) False,
  Domain.Cell 55 6 1 1 (-1) False,
  Domain.Cell 56 6 2 9 (-1) False,
  Domain.Cell 57 6 3 3 (-1) False,
  Domain.Cell 58 6 4 2 (-1) False,
  Domain.Cell 59 6 5 6 (-1) False,
  Domain.Cell 60 6 6 8 (-1) False,
  Domain.Cell 61 6 7 7 (-1) False,
  Domain.Cell 62 6 8 4 (-1) False,
  Domain.Cell 63 7 0 2 (-1) False,
  Domain.Cell 64 7 1 4 (-1) False,
  Domain.Cell 65 7 2 8 (-1) False,
  Domain.Cell 66 7 3 9 (-1) False,
  Domain.Cell 67 7 4 5 (-1) False,
  Domain.Cell 68 7 5 7 (-1) False,
  Domain.Cell 69 7 6 1 (-1) False,
  Domain.Cell 70 7 7 3 (-1) False,
  Domain.Cell 71 7 8 6 (-1) False,
  Domain.Cell 72 8 0 7 (-1) False,
  Domain.Cell 73 8 1 6 (-1) False,
  Domain.Cell 74 8 2 3 (-1) False,
  Domain.Cell 75 8 3 4 (-1) False,
  Domain.Cell 76 8 4 1 (-1) False,
  Domain.Cell 77 8 5 8 (-1) False,
  Domain.Cell 78 8 6 2 (-1) False,
  Domain.Cell 79 8 7 5 (-1) False,
  Domain.Cell 80 8 8 5 (-1) False]
  False

gridOneReal :: Domain.Grid
gridOneReal = Domain.Grid 0
  [Domain.Cell 0 0 0 (-1) (-1) False,
  Domain.Cell 1 0 1 (-1) (-1) False,
  Domain.Cell 2 0 2 (-1) (-1) False,
  Domain.Cell 3 0 3 2 (-1) False,
  Domain.Cell 4 0 4 6 (-1) False,
  Domain.Cell 5 0 5 (-1) (-1) False,
  Domain.Cell 6 0 6 7 (-1) False,
  Domain.Cell 7 0 7 (-1) (-1) False,
  Domain.Cell 8 0 8 1 (-1) False,
  Domain.Cell 9 1 0 6 (-1) False,
  Domain.Cell 10 1 1 8 (-1) False,
  Domain.Cell 11 1 2 (-1) (-1) False,
  Domain.Cell 12 1 3 (-1) (-1) False,
  Domain.Cell 13 1 4 7 (-1) False,
  Domain.Cell 14 1 5 (-1) (-1) False,
  Domain.Cell 15 1 6 (-1) (-1) False,
  Domain.Cell 16 1 7 9 (-1) False,
  Domain.Cell 17 1 8 (-1) (-1) False,
  Domain.Cell 18 2 0 1 (-1) False,
  Domain.Cell 19 2 1 9 (-1) False,
  Domain.Cell 20 2 2 (-1) (-1) False,
  Domain.Cell 21 2 3 (-1) (-1) False,
  Domain.Cell 22 2 4 (-1) (-1) False,
  Domain.Cell 23 2 5 4 (-1) False,
  Domain.Cell 24 2 6 5 (-1) False,
  Domain.Cell 25 2 7 (-1) (-1) False,
  Domain.Cell 26 2 8 (-1) (-1) False,
  Domain.Cell 27 3 0 8 (-1) False,
  Domain.Cell 28 3 1 2 (-1) False,
  Domain.Cell 29 3 2 (-1) (-1) False,
  Domain.Cell 30 3 3 1 (-1) False,
  Domain.Cell 31 3 4 (-1) (-1) False,
  Domain.Cell 32 3 5 (-1) (-1) False,
  Domain.Cell 33 3 6 (-1) (-1) False,
  Domain.Cell 34 3 7 4 (-1) False,
  Domain.Cell 35 3 8 (-1) (-1) False,
  Domain.Cell 36 4 0 (-1) (-1) False,
  Domain.Cell 37 4 1 (-1) (-1) False,
  Domain.Cell 38 4 2 4 (-1) False,
  Domain.Cell 39 4 3 6 (-1) False,
  Domain.Cell 40 4 4 (-1) (-1) False,
  Domain.Cell 41 4 5 2 (-1) False,
  Domain.Cell 42 4 6 9 (-1) False,
  Domain.Cell 43 4 7 (-1) (-1) False,
  Domain.Cell 44 4 8 (-1) (-1) False,
  Domain.Cell 45 5 0 (-1) (-1) False,
  Domain.Cell 46 5 1 5 (-1) False,
  Domain.Cell 47 5 2 (-1) (-1) False,
  Domain.Cell 48 5 3 (-1) (-1) False,
  Domain.Cell 49 5 4 (-1) (-1) False,
  Domain.Cell 50 5 5 3 (-1) False,
  Domain.Cell 51 5 6 (-1) (-1) False,
  Domain.Cell 52 5 7 2 (-1) False,
  Domain.Cell 53 5 8 8 (-1) False,
  Domain.Cell 54 6 0 (-1) (-1) False,
  Domain.Cell 55 6 1 (-1) (-1) False,
  Domain.Cell 56 6 2 9 (-1) False,
  Domain.Cell 57 6 3 3 (-1) False,
  Domain.Cell 58 6 4 (-1) (-1) False,
  Domain.Cell 59 6 5 (-1) (-1) False,
  Domain.Cell 60 6 6 (-1) (-1) False,
  Domain.Cell 61 6 7 7 (-1) False,
  Domain.Cell 62 6 8 4 (-1) False,
  Domain.Cell 63 7 0 (-1) (-1) False,
  Domain.Cell 64 7 1 4 (-1) False,
  Domain.Cell 65 7 2 (-1) (-1) False,
  Domain.Cell 66 7 3 (-1) (-1) False,
  Domain.Cell 67 7 4 5 (-1) False,
  Domain.Cell 68 7 5 (-1) (-1) False,
  Domain.Cell 69 7 6 (-1) (-1) False,
  Domain.Cell 70 7 7 3 (-1) False,
  Domain.Cell 71 7 8 6 (-1) False,
  Domain.Cell 72 8 0 7 (-1) False,
  Domain.Cell 73 8 1 (-1) (-1) False,
  Domain.Cell 74 8 2 3 (-1) False,
  Domain.Cell 75 8 3 (-1) (-1) False,
  Domain.Cell 76 8 4 1 (-1) False,
  Domain.Cell 77 8 5 8 (-1) False,
  Domain.Cell 78 8 6 (-1) (-1) False,
  Domain.Cell 79 8 7 (-1) (-1) False,
  Domain.Cell 80 8 8 (-1) (-1) False]
  False

gridTwoSolved :: Domain.Grid
gridTwoSolved = Domain.Grid 0
  [Domain.Cell 0 0 0 5  (-1) False,
  Domain.Cell 1 0 1 8 (-1) False,
  Domain.Cell 2 0 2 1 (-1) False,
  Domain.Cell 3 0 3 6 (-1) False,
  Domain.Cell 4 0 4 7 (-1) False,
  Domain.Cell 5 0 5 2 (-1) False,
  Domain.Cell 6 0 6 4 (-1) False,
  Domain.Cell 7 0 7 3 (-1) False,
  Domain.Cell 8 0 8 9 (-1) False,
  Domain.Cell 9 1 0 7 (-1) False,
  Domain.Cell 10 1 1 9 (-1) False,
  Domain.Cell 11 1 2 2 (-1) False,
  Domain.Cell 12 1 3 8 (-1) False,
  Domain.Cell 13 1 4 4 (-1) False,
  Domain.Cell 14 1 5 3 (-1) False,
  Domain.Cell 15 1 6 6 (-1) False,
  Domain.Cell 16 1 7 5 (-1) False,
  Domain.Cell 17 1 8 1 (-1) False,
  Domain.Cell 18 2 0 3 (-1) False,
  Domain.Cell 19 2 1 6 (-1) False,
  Domain.Cell 20 2 2 4 (-1) False,
  Domain.Cell 21 2 3 5 (-1) False,
  Domain.Cell 22 2 4 9 (-1) False,
  Domain.Cell 23 2 5 1 (-1) False,
  Domain.Cell 24 2 6 7 (-1) False,
  Domain.Cell 25 2 7 8 (-1) False,
  Domain.Cell 26 2 8 2 (-1) False,
  Domain.Cell 27 3 0 4 (-1) False,
  Domain.Cell 28 3 1 3 (-1) False,
  Domain.Cell 29 3 2 8 (-1) False,
  Domain.Cell 30 3 3 9 (-1) False,
  Domain.Cell 31 3 4 5 (-1) False,
  Domain.Cell 32 3 5 7 (-1) False,
  Domain.Cell 33 3 6 2 (-1) False,
  Domain.Cell 34 3 7 1 (-1) False,
  Domain.Cell 35 3 8 6 (-1) False,
  Domain.Cell 36 4 0 2 (-1) False,
  Domain.Cell 37 4 1 5 (-1) False,
  Domain.Cell 38 4 2 6 (-1) False,
  Domain.Cell 39 4 3 1 (-1) False,
  Domain.Cell 40 4 4 8 (-1) False,
  Domain.Cell 41 4 5 4 (-1) False,
  Domain.Cell 42 4 6 9 (-1) False,
  Domain.Cell 43 4 7 7 (-1) False,
  Domain.Cell 44 4 8 3 (-1) False,
  Domain.Cell 45 5 0 1 (-1) False,
  Domain.Cell 46 5 1 7 (-1) False,
  Domain.Cell 47 5 2 9 (-1) False,
  Domain.Cell 48 5 3 3 (-1) False,
  Domain.Cell 49 5 4 2 (-1) False,
  Domain.Cell 50 5 5 6 (-1) False,
  Domain.Cell 51 5 6 8 (-1) False,
  Domain.Cell 52 5 7 4 (-1) False,
  Domain.Cell 53 5 8 5 (-1) False,
  Domain.Cell 54 6 0 8 (-1) False,
  Domain.Cell 55 6 1 4 (-1) False,
  Domain.Cell 56 6 2 5 (-1) False,
  Domain.Cell 57 6 3 2 (-1) False,
  Domain.Cell 58 6 4 1 (-1) False,
  Domain.Cell 59 6 5 9 (-1) False,
  Domain.Cell 60 6 6 3 (-1) False,
  Domain.Cell 61 6 7 6 (-1) False,
  Domain.Cell 62 6 8 7 (-1) False,
  Domain.Cell 63 7 0 9 (-1) False,
  Domain.Cell 64 7 1 1 (-1) False,
  Domain.Cell 65 7 2 3 (-1) False,
  Domain.Cell 66 7 3 7 (-1) False,
  Domain.Cell 67 7 4 6 (-1) False,
  Domain.Cell 68 7 5 8 (-1) False,
  Domain.Cell 69 7 6 5 (-1) False,
  Domain.Cell 70 7 7 2 (-1) False,
  Domain.Cell 71 7 8 4 (-1) False,
  Domain.Cell 72 8 0 6 (-1) False,
  Domain.Cell 73 8 1 2 (-1) False,
  Domain.Cell 74 8 2 7 (-1) False,
  Domain.Cell 75 8 3 4 (-1) False,
  Domain.Cell 76 8 4 3 (-1) False,
  Domain.Cell 77 8 5 5 (-1) False,
  Domain.Cell 78 8 6 1 (-1) False,
  Domain.Cell 79 8 7 9 (-1) False,
  Domain.Cell 80 8 8 8 (-1) False]
  False

gridTwoReal :: Domain.Grid
gridTwoReal = Domain.Grid 0
  [Domain.Cell 0 0 0 (-1) (-1) False,
  Domain.Cell 1 0 1 (-1) (-1) False,
  Domain.Cell 2 0 2 (-1) (-1) False,
  Domain.Cell 3 0 3 6 (-1) False,
  Domain.Cell 4 0 4 (-1) (-1) False,
  Domain.Cell 5 0 5 (-1) (-1) False,
  Domain.Cell 6 0 6 4 (-1) False,
  Domain.Cell 7 0 7 (-1) (-1) False,
  Domain.Cell 8 0 8 (-1) (-1) False,
  Domain.Cell 9 1 0 7 (-1) False,
  Domain.Cell 10 1 1 (-1) (-1) False,
  Domain.Cell 11 1 2 (-1) (-1) False,
  Domain.Cell 12 1 3 (-1) (-1) False,
  Domain.Cell 13 1 4 (-1) (-1) False,
  Domain.Cell 14 1 5 3 (-1) False,
  Domain.Cell 15 1 6 6 (-1) False,
  Domain.Cell 16 1 7 (-1) (-1) False,
  Domain.Cell 17 1 8 (-1) (-1) False,
  Domain.Cell 18 2 0 (-1) (-1) False,
  Domain.Cell 19 2 1 (-1) (-1) False,
  Domain.Cell 20 2 2 (-1) (-1) False,
  Domain.Cell 21 2 3 (-1) (-1) False,
  Domain.Cell 22 2 4 9 (-1) False,
  Domain.Cell 23 2 5 1 (-1) False,
  Domain.Cell 24 2 6 (-1) (-1) False,
  Domain.Cell 25 2 7 8 (-1) False,
  Domain.Cell 26 2 8 (-1) (-1) False,
  Domain.Cell 27 3 0 (-1) (-1) False,
  Domain.Cell 28 3 1 (-1) (-1) False,
  Domain.Cell 29 3 2 (-1) (-1) False,
  Domain.Cell 30 3 3 (-1) (-1) False,
  Domain.Cell 31 3 4 (-1) (-1) False,
  Domain.Cell 32 3 5 (-1) (-1) False,
  Domain.Cell 33 3 6 (-1) (-1) False,
  Domain.Cell 34 3 7 (-1) (-1) False,
  Domain.Cell 35 3 8 (-1) (-1) False,
  Domain.Cell 36 4 0 (-1) (-1) False,
  Domain.Cell 37 4 1 5 (-1) False,
  Domain.Cell 38 4 2 (-1) (-1) False,
  Domain.Cell 39 4 3 1 (-1) False,
  Domain.Cell 40 4 4 8 (-1) False,
  Domain.Cell 41 4 5 (-1) (-1) False,
  Domain.Cell 42 4 6 (-1) (-1) False,
  Domain.Cell 43 4 7 (-1) (-1) False,
  Domain.Cell 44 4 8 3 (-1) False,
  Domain.Cell 45 5 0 (-1) (-1) False,
  Domain.Cell 46 5 1 (-1) (-1) False,
  Domain.Cell 47 5 2 (-1) (-1) False,
  Domain.Cell 48 5 3 3 (-1) False,
  Domain.Cell 49 5 4 (-1) (-1) False,
  Domain.Cell 50 5 5 6 (-1) False,
  Domain.Cell 51 5 6 (-1) (-1) False,
  Domain.Cell 52 5 7 4 (-1) False,
  Domain.Cell 53 5 8 5 (-1) False,
  Domain.Cell 54 6 0 (-1) (-1) False,
  Domain.Cell 55 6 1 4 (-1) False,
  Domain.Cell 56 6 2 (-1) (-1) False,
  Domain.Cell 57 6 3 2 (-1) False,
  Domain.Cell 58 6 4 (-1) (-1) False,
  Domain.Cell 59 6 5 (-1) (-1) False,
  Domain.Cell 60 6 6 (-1) (-1) False,
  Domain.Cell 61 6 7 6 (-1) False,
  Domain.Cell 62 6 8 (-1) (-1) False,
  Domain.Cell 63 7 0 9 (-1) False,
  Domain.Cell 64 7 1 (-1) (-1) False,
  Domain.Cell 65 7 2 3 (-1) False,
  Domain.Cell 66 7 3 (-1) (-1) False,
  Domain.Cell 67 7 4 (-1) (-1) False,
  Domain.Cell 68 7 5 (-1) (-1) False,
  Domain.Cell 69 7 6 (-1) (-1) False,
  Domain.Cell 70 7 7 (-1) (-1) False,
  Domain.Cell 71 7 8 (-1) (-1) False,
  Domain.Cell 72 8 0 (-1) (-1) False,
  Domain.Cell 73 8 1 2 (-1) False,
  Domain.Cell 74 8 2 (-1) (-1) False,
  Domain.Cell 75 8 3 (-1) (-1) False,
  Domain.Cell 76 8 4 (-1) (-1) False,
  Domain.Cell 77 8 5 (-1) (-1) False,
  Domain.Cell 78 8 6 1 (-1) False,
  Domain.Cell 79 8 7 (-1) (-1) False,
  Domain.Cell 80 8 8 (-1) (-1) False]
  False