-- Day 7: Bridge Repair
module Day7 (part1, part2) where

import Data.List (uncons)
import Helpers (splitOn)

type Calibration = (Integer, [Integer])
type Operation = (Integer -> Integer -> Integer)

-- Total calibration result with two operations: (+) and (*).
part1 :: String -> Integer
part1 input = sumCorrect [(+), (*)] $ parse input

-- Total calibration result with three operations: (+), (*), and (||).
part2 :: String -> Integer
part2 input = sumCorrect [(+), (*), concatenate] $ parse input
  where
    concatenate :: Integer -> Integer -> Integer
    concatenate a b = read (show a <> show b) :: Integer

sumCorrect :: [Operation] -> [Calibration] -> Integer
sumCorrect ops calibrations = sum . map fst . filter isCorrect $ calibrations
  where
    -- Check if an equations is correct, i.e. returns expected result.
    isCorrect (expected, nums) =
      let Just (n,ns) = uncons nums
      in any (== expected) (calcResults [n] ns)

    -- Return result of calculation with all variants of the given operations.
    calcResults acc [] = acc
    calcResults acc (n:ns) =
      let acc' = concat $ map (applyOpToAll n acc) ops
       in calcResults acc' ns

    applyOpToAll n ms op = map (\m -> op m n) ms


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
