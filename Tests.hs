import CB
import Control.Monad.Trans.Maybe
import Data.Maybe (catMaybes)
import Nonogram
import Nonogram (initNonogram)

test1 :: [Nonogram]
test1 = cb [[1]] [[1]]

-- Expected output: [[[Black]]]

-- Test case 2: 1x1 nonogram expecting a single white cell.
test2 :: [Nonogram]
test2 = cb [[]] [[]]

-- Expected output: [[[White]]]

-- Test case 3: 2x2 nonogram with one black cell per row and per column.
test3 :: [Nonogram]
test3 = cb [[1], [1]] [[1], [1]]

-- Expected output: There are two possible solutions.
-- One possibility:
--   [ [Black, White]
--   , [White, Black] ]
-- Another possibility:
--   [ [White, Black]
--   , [Black, White] ]

-- Test case 4: The provided 9x8 puzzle.
test4 :: [Nonogram]
test4 =
  cb
    [[3], [2, 1], [3, 2], [2, 2], [6], [1, 5], [6], [1], [2]]
    [[1, 2], [3, 1], [1, 5], [7, 1], [5], [3], [4], [3]]

-- Expected output: A valid 9x8 solution matching these clues.

-- Test case 5: A contradictory puzzle.
-- Row demands one black cell while the column clue demands none.
test5 :: [Nonogram]
test5 = cb [[1]] [[]]

-- Expected output: [] (i.e. no solution)

test6 :: [Nonogram]
test6 = cb [[3], [1, 1], [2], [3], [3]] [[2], [1], [2, 2], [3], [3]]

test7 :: [Nonogram]
test7 =
  cb
    [ [14],
      [1, 1],
      [7, 1],
      [3, 3],
      [2, 3, 2],
      [2, 3, 2],
      [1, 3, 6, 1, 1],
      [1, 8, 2, 1],
      [1, 4, 6, 1],
      [1, 3, 2, 5, 1, 1],
      [1, 5, 1],
      [2, 2],
      [2, 1, 1, 1, 2],
      [6, 5, 3],
      [12]
    ]
    [ [7],
      [2, 2],
      [2, 2],
      [2, 1, 1, 1, 1],
      [1, 2, 4, 2],
      [1, 1, 4, 2],
      [1, 1, 2, 3],
      [1, 1, 3, 2],
      [1, 1, 1, 2, 2, 1],
      [1, 1, 5, 1, 2],
      [1, 1, 7, 2],
      [1, 6, 3],
      [1, 1, 3, 2],
      [1, 4, 3],
      [1, 3, 1],
      [1, 2, 2],
      [2, 1, 1, 1, 1],
      [2, 2],
      [2, 2],
      [7]
    ]

test8 :: [Nonogram]
test8 =
  cb
    [[2, 1, 1], [4], [4], [2], [4], [2, 2, 3], [1, 3], [3, 4], [7], [4, 3]]
    [[3, 1], [3, 1], [3, 1, 1], [4, 5], [1, 3], [2], [5], [6], [1, 6], [1, 3]]

test9 :: [Nonogram]
test9 =
  cb
    [ [11],
      [10],
      [2, 4],
      [4],
      [1, 3, 1],
      [6, 5],
      [13],
      [4, 2, 3],
      [5, 2],
      [5, 3],
      [1, 4],
      [5],
      [2, 4],
      [2, 3, 2],
      [2, 3, 1]
    ]
    [ [1, 6, 3],
      [1, 1, 5, 3],
      [2, 5],
      [3, 6, 2],
      [3, 3, 3, 2],
      [3, 3, 2],
      [3, 2],
      [2, 1, 3],
      [2, 1, 2],
      [2, 4, 2, 1],
      [2, 1, 3, 3],
      [2, 8],
      [3, 4],
      [3],
      [2]
    ]
