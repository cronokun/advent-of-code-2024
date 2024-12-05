-- description: Day 1: Historian Hysteria

module Day01 (part1, part2) where

import Data.List (sort)
import qualified Data.Map as Map

parse :: String -> [[Integer]]
parse input =
  let parsedNums = map read $ words input :: [Integer]
  in split [[], []] parsedNums
  where
    split acc [] = acc
    split [as, bs] (a:b:rest) = split [a:as, b:bs] rest

-- Total distance between lists

part1 :: String -> Integer
part1 input =
  calculateDiff $ parse input
  where
    calculateDiff [as, bs] =
      foldr sumPairDiffs 0 $ pairs
      where
        sumPairDiffs = (+) . abs . uncurry(-)
        pairs = zip (sort as) (sort bs)

-- Lists similarity score

part2 :: String -> Integer
part2 input =
  let [as, bs] = parse input
      cs = count bs
  in similarityScore as cs
  where
    similarityScore as cs = foldr (\a -> (+) (score a cs)) 0 as
    score x cs = x * Map.findWithDefault 0 x cs
    count = foldr (\x -> Map.insertWith (+) x 1) Map.empty
