{-# LANGUAGE LambdaCase #-}

module DFS where

import Data.List

type Row = [Bool]

type Column = [Bool]

type Nonogram = [Row]

type Clue = [Int]

showSquare :: Bool -> Char
showSquare = \case
  True -> 'X'
  False -> '_'

showRow :: Row -> String
showRow row = '|' : (intersperse '|' . map showSquare $ row) ++ ['|']

showNonogram :: Nonogram -> String
showNonogram = unlines . map showRow

annotateColumn :: Column -> Clue
annotateColumn = map length . filter and . group

checkColumn :: Clue -> Column -> Bool
checkColumn clue col = clue == annotateColumn col

checkNonogram :: [Clue] -> Nonogram -> Bool
checkNonogram clues nono = and $ zipWith checkColumn clues (transpose nono)

fillRow :: Int -> Clue -> [Row]
fillRow col_dim [] = [replicate col_dim False]
fillRow col_dim [n] =
  [ replicate i False ++ replicate n True ++ replicate (col_dim - i - n) False
    | i <- [0 .. col_dim - n]
  ]
fillRow col_dim (n : ns) =
  [ replicate i False ++ replicate n True ++ [False] ++ rest
    | i <- [0 .. col_dim - n - 1],
      rest <- fillRow (col_dim - i - n - 1) ns
  ]

fillNonogram :: Int -> [Clue] -> [Nonogram]
fillNonogram col_dim = mapM (fillRow col_dim)

dfs :: [Clue] -> [Clue] -> [Nonogram]
dfs row_clues col_clues =
  filter (checkNonogram col_clues) $ fillNonogram (length col_clues) row_clues
