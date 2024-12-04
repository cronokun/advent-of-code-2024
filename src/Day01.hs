-- description: Day 1: Historian Hysteria

module Day01 (part1) where

import Data.List (sort)

part1 :: String -> Integer
part1 input =
  calculateDiff parsedInput
  where
    calculateDiff [as, bs] =
      foldr sumPairDiffs 0 $ pairs
      where
        sumPairDiffs = (+) . abs . uncurry(-)
        pairs = zip (sort as) (sort bs)

    parsedInput =
      let parsedNums = map read $ words input :: [Integer]
      in split [[], []] parsedNums
      where
        split acc [] = acc
        split [as, bs] (a:b:rest) = split [a:as, b:bs] rest
