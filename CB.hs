{-# LANGUAGE LambdaCase #-}

module CB where

import Control.Monad
import Control.Monad.Trans.Maybe
import Data.List
import Data.Maybe
import Nonogram
import Rules

cb :: [Clue] -> [Clue] -> [Nonogram]
cb rowClues colClues =
  let numCols = length colClues
      constraints = toConstraints numCols rowClues
      nono = initNonogram (length rowClues) (length colClues)
   in cb' colClues (constraints, nono)

cb' :: [Clue] -> ([Constraint], Nonogram) -> [Nonogram]
cb' colClues (constraints, nono0) =
  let numCols = length colClues
      (newConstraints, nono) = applyAllRules nono0 constraints
   in -- (newConstraints, nono) = (constraints, nono0)
      if complete nono
        then
          if checkNonogram colClues nono then [nono] else []
        else do
          child <- getChildren newConstraints nono
          guard (checkNonogram colClues child)
          cb' colClues (newConstraints, child)
  where
    complete = all (Unknown `notElem`)

dfs :: [Clue] -> [Clue] -> [Nonogram]
dfs rowClues colClues =
  let constraints = toConstraints (length colClues) rowClues
      rs = fillRow <$> constraints <*> pure (replicate (length colClues) Unknown)
      nonos = sequence rs
   in filter (checkNonogram colClues) nonos

annotateColumn :: Line -> Clue
annotateColumn = map length . filter (Black `elem`) . group

-- check if column is compatible with clue
checkColumn :: Clue -> Line -> Bool
checkColumn clue col =
  let pb = map (\c -> if c == Unknown then [White, Black] else [c]) col
      annotated = map annotateColumn $ sequence pb
   in clue `elem` annotated

checkNonogram :: [Clue] -> Nonogram -> Bool
checkNonogram cs = and . zipWith checkColumn cs . transpose

fillRun :: Int -> Int -> Line -> [Line]
fillRun s l = zipWithM fillCell [0 ..]
  where
    inRun i = s <= i && i <= (s + l - 1)
    fillCell i c
      | inRun i = [Black | c == Unknown || c == Black]
      | otherwise = [c]

fillRow :: Constraint -> Line -> [Line]
fillRow [] row = return $ map (\c -> if c == Unknown then White else c) row
fillRow ((Run l s e) : rs) row = do
  i <- [s .. e - l + 1]
  newRow <- fillRun i l row
  fillRow rs newRow

getChildren :: [Constraint] -> Nonogram -> [Nonogram]
getChildren cs nono =
  do
    let index = case findIndex (Unknown `elem`) nono of
          Just j -> [j]
          _ -> []
    i <- index
    row <- fillRow (cs !! i) (nono !! i)
    return $ zipWith (\j r -> if j == i then row else r) [0 ..] nono