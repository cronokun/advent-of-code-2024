-- description: Day 2: Red-Nosed Reports

module Day2 (part1, part2) where

import Data.List (drop, take)

parse :: String -> [[Integer]]
parse input =
  map toIntList $ lines input
  where
    toIntList line = map read $ words line :: [Integer]


countSafe :: ([Integer] -> Bool) -> [[Integer]] -> Integer
countSafe f records = toInteger . length . filter f $ records

isSafe xs =
  let diffsOk = all (\x -> abs x >= 1 && abs x <= 3) diffs
      signsOk = all sameSign $ zip diffs (tail diffs)
      diffs = map (uncurry (-)) $ zip xs (tail xs)
   in diffsOk && signsOk
  where
    sameSign (a, b)
      | a * b > 0 = True
      | otherwise = False

isSafe' xs = any isSafe $ variants xs
  where
    dropAt n xs = take (n - 1) xs <> drop (n) xs
    variants xs = (xs : [dropAt i xs | i <- [1..(length xs)]])


-- Number of safe reports.

part1 :: String -> Integer
part1 input = countSafe isSafe $ parse input

-- Number of safe report with single bad level

part2 :: String -> Integer
part2 input = countSafe isSafe' $ parse input