module Nonogram where

data Cell = Unknown | White | Black deriving (Eq, Show)

type Line = [Cell]

type Nonogram = [Line]

type Clue = [Int]

data Run = Run
  { runLength :: Int,
    runStart :: Int,
    runEnd :: Int
  }
  deriving (Eq, Show)

-- a constraint is a list of black runs with runranges
type Constraint = [Run]

initNonogram :: Int -> Int -> Nonogram
initNonogram numRows numCols = replicate numRows (replicate numCols Unknown)

toConstraint :: Int -> Clue -> Constraint
toConstraint numCols clue =
  [ Run len start end
    | (j, len) <- zip [1 ..] clue,
      let start =
            if j == 1
              then 0
              else sum (map (+ 1) (take (j - 1) clue))
          end =
            if j == length clue
              then numCols - 1
              else (numCols - 1) - sum (map (+ 1) (drop j clue))
  ]

toConstraints :: Int -> [Clue] -> [Constraint]
toConstraints n = map (toConstraint n)

renderCell :: Cell -> Char
renderCell Unknown = '?'
renderCell Black = 'X'
renderCell White = '_'

renderRow :: Line -> String
renderRow = map renderCell

renderNonogram :: Nonogram -> String
renderNonogram = unlines . map renderRow
