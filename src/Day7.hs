-- Day 7: Bridge Repair
module Day7 (part1, part2) where

import Helpers (splitOn)

type Calibration = (Integer, [Integer])

-- Total calibration result with two operations: (+) and (*).
part1 :: String -> Integer
part1 input = sumCorrect allResults $ parse input

-- Total calibration result with three operations: (+), (*), and (||).
part2 :: String -> Integer
part2 input = sumCorrect allResults' $ parse input

sumCorrect :: ([Integer] -> [Integer]) -> [Calibration] -> Integer
sumCorrect f calibrations = sum . map fst . filter (isCorrect f) $ calibrations


-- Check if an equations is correct, i.e. returns expected result.
isCorrect :: ([Integer] -> [Integer]) -> Calibration -> Bool
isCorrect f (exp, nums) = any (== exp) (f nums)

-- Calculates results for all possible combinations of (+) and (*) operators.
allResults :: [Integer] -> [Integer]
allResults (num:rest) = calcResults [num] rest
  where
    calcResults acc [] = acc
    calcResults acc (n:ns) =
      let acc1 = map (+ n) acc
          acc2 = map (* n ) acc
          acc' = acc1 <> acc2
       in calcResults acc' ns

-- Calculates results for all possible combinations of (+), (*) and (||) operators.
allResults' :: [Integer] -> [Integer]
allResults' (num:rest) = calcResults [num] rest
  where
    calcResults acc [] = acc
    calcResults acc (n:ns) =
      let acc1 = map (+ n) acc
          acc2 = map (* n ) acc
          acc3 = map (flip concatenate n) acc
          acc' = acc1 <> acc2 <> acc3
       in calcResults acc' ns

    concatenate a b = read (show a <> show b) :: Integer

-- Parse input data
parse :: String -> [Calibration]
parse input = map parseLine . lines $ input
  where
    parseLine :: String -> (Integer, [Integer])
    parseLine line =
      let [a, b] = splitOn ':' line
          expected = read a :: Integer
          nums = map read $ words b :: [Integer]
       in (expected, nums)
