-- Day 2: Red-Nosed Reports
module Day2 (part1, part2) where

-- Number of safe reports.
part1 :: String -> Int
part1 input = countSafe isSafe $ parse input

-- Number of safe report with single bad level
part2 :: String -> Int
part2 input = countSafe isSafe' $ parse input

parse :: String -> [[Int]]
parse input =
  map toIntList $ lines input
  where
    toIntList line = map read $ words line :: [Int]


countSafe :: ([Int] -> Bool) -> [[Int]] -> Int
countSafe f records = length . filter f $ records

isSafe :: [Int] -> Bool
isSafe xs =
  let diffsOk = all (\x -> abs x >= 1 && abs x <= 3) diffs
      signsOk = all sameSign $ zip diffs (drop 1 diffs)
      diffs = map (uncurry (-)) $ zip xs (drop 1 xs)
   in diffsOk && signsOk
  where
    sameSign (a, b)
      | a * b > 0 = True
      | otherwise = False

isSafe' :: [Int] -> Bool
isSafe' lst = any isSafe $ variants lst
  where
    dropAt n xs = take (n - 1) xs <> drop (n) xs
    variants xs = (xs : [dropAt i xs | i <- [1..(length xs)]])
