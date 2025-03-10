-- Day 7: Bridge Repair
module Day7 (part1, part2) where

import Data.List (uncons)
import Helpers (splitOn)

type Calibration = (Int, [Int])
type Operation = (Int -> Int -> Int)

-- Total calibration result with two operations: (+) and (*).
part1 :: String -> Int
part1 input = sumCorrect [(+), (*)] $ parse input

-- Total calibration result with three operations: (+), (*), and (||).
part2 :: String -> Int
part2 input = sumCorrect [(+), (*), concat'] $ parse input
  where
    concat' :: Int -> Int -> Int
    concat' x y = foldl' (\acc d -> acc * 10 + d) x (reverse $ digits y)
      where
        digits :: Int -> [Int]
        digits 0 = []
        digits n = (n `mod` 10) : digits (n `div` 10)

sumCorrect :: [Operation] -> [Calibration] -> Int
sumCorrect ops calibrations = sum . map fst . filter isCorrect $ calibrations
  where
    -- Check if an equations is correct, i.e. returns expected result.
    isCorrect (expected, nums) =
      let Just (n,ns) = uncons nums
      in any (== expected) (calcResults expected [n] ns)

    -- Return result of calculation with all variants of the given operations.
    calcResults expected acc [] = acc
    calcResults expected acc (n:ns) =
      let acc' = concatMap (applyOpToAll n acc) ops
          acc'' = filter (<= expected) acc' -- filter out values that already incorrect
       in calcResults expected acc'' ns

    applyOpToAll n ms op = map (\m -> op m n) ms

-- Parse input data
parse :: String -> [Calibration]
parse input = map parseLine . lines $ input
  where
    parseLine :: String -> (Int, [Int])
    parseLine line =
      let [a, b] = splitOn ':' line
          expected = read a :: Int
          nums = map read $ words b :: [Int]
       in (expected, nums)
