-- Day 11: Plutonian Pebbles
module Day11 (part1, part2) where

import qualified Data.List as List
import qualified Data.Map as Map

-- How many stones will you have after blinking 25 times?
part1 :: String -> Int
part1 input = length . process 25 . parse $ input

-- How many stones will you have after blinking 75 times?
part2 :: String -> Int
part2 input = process' . map (\x -> (x, 75, 1)) $ parse input

process :: Int -> [Int] -> [Int]
process 0 xs = xs
process c nums = process (c - 1) $ helper [] nums
  where
    helper :: [Int] -> [Int] -> [Int]
    helper acc [] = reverse acc
    helper acc (0:xs) = helper (1:acc) xs
    helper acc (x:xs) =
      if hasEvenDigits x
      then let (l, r) = splitNum x in helper (r:l:acc) xs
      else helper ((x * 2024) : acc) xs

process' :: [(Int, Int, Int)] -> Int
process' = helper 0
  where
    helper :: Int -> [(Int, Int, Int)] -> Int
    helper acc [] = acc
    helper acc xs =
      let xs' = concatMap split xs
          (done, notdone) = splitByDone xs'
          acc' = foldr sumCounts acc done
          next = comb notdone
       in helper acc' next

    -- Combine stones with the same number and blinks left.
    comb = fromM . toM
    fromM = map removeKey . Map.toList
    toM = Map.fromListWith (+) . map makeKey
    makeKey = (\(a,b,c) -> ((a,b),c))
    removeKey = (\((a,b),c) -> (a,b,c))
    sumCounts = (\(_, _, k) total -> total + k)
    splitByDone = List.partition (\(_, n, _) -> n == 0)

    split :: (Int, Int, Int) -> [(Int, Int, Int)]
    split (0, n, k) = [(1, n - 1, k)]
    split (x, n, k)
      | hasEvenDigits x = 
          let (l, r) = splitNum x
           in if l == r
              then [(l, n - 1, 2 * k)]
              else [(l, n - 1, k), (r, n - 1, k)]
      | otherwise = [(x * 2024, n - 1, k)]

hasEvenDigits :: Int -> Bool
hasEvenDigits x = even . numberOfDigits $ x

numberOfDigits :: Int -> Int
numberOfDigits num = helper 1 num
  where
    helper acc n =
      case n `div` 10 of
        0 -> acc
        m -> helper (acc + 1) m

splitNum :: Int -> (Int, Int)
splitNum x =
  let len = numberOfDigits x
      d = 10 ^ (len `div` 2)
      l = x `div` d
      r = x `rem` d
 in (l, r)

parse :: String -> [Int]
parse = map (\x -> read x :: Int) . words
