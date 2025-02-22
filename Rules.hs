module Rules where

import Control.Applicative (Const)
import Data.Data (Constr)
import Data.List
import Nonogram

type Rule = Line -> Constraint -> Line

runLengths :: Constraint -> [Int]
runLengths = map runLength

runStarts :: Constraint -> [Int]
runStarts = map runStart

runEnds :: Constraint -> [Int]
runEnds = map runEnd

-- Only updates cell when it is unknown
updateRow :: Int -> Cell -> Line -> Line
updateRow i c row = if row !! i == Unknown then take i row ++ [c] ++ drop (i + 1) row else row

-- For each black run, those cells in the intersection of
-- all the possible solutions of the black run must be colored
rule1_1 :: Line -> Constraint -> Line
rule1_1 row [] = row
rule1_1 row cs =
  zipWith update [0 ..] row
  where
    runs = [(s, e, u) | Run l s e <- cs, let u = (e - s + 1) - l]
    inIntersection i = any (\(s, e, u) -> s + u <= i && i <= e - u) runs
    update i Unknown
      | inIntersection i = Black
      | otherwise = Unknown
    update _ cell = cell

-- When a cell does not belong to the run range of
-- any black run, the cell should be left empty
rule1_2 :: Line -> Constraint -> Line
rule1_2 row [] = row
rule1_2 row cs =
  zipWith update [0 ..] row
  where
    ranges = [(start, end) | Run len start end <- cs]
    notInRun i = all (\(s, e) -> i < s || i > e) ranges
    update i Unknown
      | notInRun i = White
      | otherwise = Unknown
    update _ cell = cell

rule1_3 :: Line -> Constraint -> Line
rule1_3 row [] = row
rule1_3 row constraints =
  foldl update row constraints
  where
    coveringRuns i exceptRun =
      filter (\r@(Run l s e) -> r /= exceptRun && s <= i && i <= e) constraints
    allOnes = all (\(Run l _ _) -> l == 1)
    update row run@(Run l s e) =
      let coveringS = coveringRuns s run
          newRow =
            if allOnes coveringS && s > 0 && row !! s == Black
              then updateRow (s - 1) White row
              else row
          coveringE = coveringRuns e run
          finalRow =
            if allOnes coveringE && e < (length row - 1) && row !! e == Black
              then updateRow (e + 1) White newRow
              else newRow
       in finalRow

rule1_4 :: Line -> Constraint -> Line
rule1_4 row [] = row
rule1_4 row constraint = zipWith update [0 ..] row
  where
    update i Unknown
      | i > 0 && i < length row - 1 =
          let left = row !! (i - 1)
              right = row !! (i + 1)
           in if left == Black
                && right == Black
                && (findMaxLength i < findRunLength i (updateRow i Black row))
                then White
                else Unknown
    update _ cell = cell

    findMaxLength i =
      foldl max 0 [l | Run l s e <- constraint, s <= i, i <= e]

    findRunLength :: Int -> Line -> Int
    findRunLength i r = go 0 (group r)
      where
        go _ [] = 0
        go acc (g : gs)
          | i < acc + length g = if head g == Black then length g else 0
          | otherwise = go (acc + length g) gs

rule2_1 :: Constraint -> Constraint
rule2_1 cs = zipWith update [0 ..] cs
  where
    update i r@(Run l s e) =
      let newStart =
            if i > 0
              then
                let left = (cs !! (i - 1))
                 in if s <= runStart left then runStart left + runLength left + 1 else s
              else s

          newEnd =
            if i < length cs - 1
              then
                let right = (cs !! (i + 1))
                 in if e >= runEnd right then runEnd right - runLength right - 1 else e
              else e
       in Run l newStart newEnd

rule2_2 :: Line -> Constraint -> Constraint
rule2_2 row = map updateRun
  where
    updateRun (Run l s e) =
      let newS =
            if s > 0 && isColored (row !! (s - 1))
              then s + 1
              else s
          newE =
            if e < length row - 1 && isColored (row !! (e + 1))
              then e - 1
              else e
       in Run l newS newE

isColored :: Cell -> Bool
isColored Black = True
isColored _ = False

-- applyRules :: Nonogram -> [Constraint] -> ([Constraint], Nonogram)
-- applyRules nono constraints = (map applyRule2 constraints, zipWith applyRule1 nono constraints)
--   where
--     applyRule1 row cs = foldl (\r rule -> rule r cs) row [rule1_1, rule1_2, rule1_3, rule1_4]
--     applyRule2 cs = foldl (\cs rule -> rule cs) cs [rule2_1, rule2_2]

applyAllRules :: Nonogram -> [Constraint] -> ([Constraint], Nonogram)
applyAllRules nono constraints =
  let -- First update each row’s constraints:
      newConstraints = zipWith (\cs row -> rule2_2 row (rule2_1 cs)) constraints nono
      -- Then update each row using the rule1 functions:
      newNono = zipWith applyRow newConstraints nono
   in (newConstraints, newNono)
  where
    applyRow cs row = foldl (\acc rule -> rule acc cs) row [rule1_1, rule1_2, rule1_3, rule1_4]