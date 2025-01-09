-- Day 7: Bridge Repair
module Day7 (part1) where

import Helpers (splitOn)

-- Total calibration result.
part1 :: String -> Integer
part1 input =
  let calibrations = parse input
   in sum . map (fst) . filter isCorrect $ calibrations

-- Check if an equations is correct, i.e. returns expected result.
isCorrect :: (Integer, [Integer]) -> Bool
isCorrect (exp, nums) =
  let results = allResults nums
   in any (== exp) results

-- Calculates results for all possible combinations of (+) and (*) operators.
allResults (num:rest) = calcResults [num] rest
  where
    calcResults acc [] = acc
    calcResults acc (n:ns) =
      let acc1 = map (+ n) acc
          acc2 = map (* n ) acc
          acc' = acc1 <> acc2
       in calcResults acc' ns

-- Parse input data
parse :: String -> [(Integer, [Integer])]
parse input = map parseLine . lines $ input
  where
    parseLine :: String -> (Integer, [Integer])
    parseLine line =
      let [a, b] = splitOn ':' line
          expected = read a :: Integer
          nums = map read $ words b :: [Integer]
       in (expected, nums)
