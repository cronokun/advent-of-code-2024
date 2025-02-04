-- Day 13: Claw Contraption
module Day13 (part1) where

import Helpers (lineGroups)

type Machine = (Int, Int, Int, Int, Int, Int)
data Mode = MOn | MOff deriving Eq

-- Returns fewest tokens needed to win all possible prizes.
part1 :: String -> Int
part1 input = sum . map minTokens . parse $ input

minTokens :: Machine -> Int
minTokens (ax, ay, bx, by, px, py) =
  let res = [ 3 * n + m
            | n <- [0..100], m <- [0..100]
            , n * ax + m * bx == px
            , n * ay + m * by == py
            ]
   in case res of
        [] -> 0
        ts -> minimum ts


parse :: String -> [Machine]
parse = map parseBlock . lineGroups
  where
    parseBlock :: [String] -> Machine
    parseBlock ls = toTuple $ concatMap parseNums ls

    parseNums :: String -> [Int]
    parseNums str = map read $ helper MOff "" [] str :: [Int]
      where
        helper :: Mode -> String -> [String] -> String -> [String]
        helper MOff _   acc "" = reverse acc
        helper MOn  num acc "" = reverse (num:acc)
        helper state num acc (x:xs)
          | isDig x      = helper MOn (num ++ [x]) acc xs
          | state == MOn = helper MOff "" (num : acc) xs
          | otherwise    = helper MOff num acc xs

        isDig :: Char -> Bool
        isDig x = x `elem` "0123456789"

    toTuple :: [Int] -> Machine
    toTuple [a,b,c,d,e,f] = (a,b,c,d,e,f)
